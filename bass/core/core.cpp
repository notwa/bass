#include "evaluate.cpp"
#include "analyze.cpp"
#include "execute.cpp"
#include "assemble.cpp"
#include "utility.cpp"

auto Bass::target(const string& filename, bool create) -> bool {
  if(targetFile) targetFile.close();
  if(!filename) return true;

  //cannot modify a file unless it exists
  if(!file::exists(filename)) create = true;

  if(!targetFile.open(filename, create ? file::mode::write : file::mode::modify)) {
    print(stderr, "warning: unable to open target file: ", filename, "\n");
    return false;
  }

  tracker.addresses.reset();
  return true;
}

auto Bass::source(const string& filename) -> bool {
  if(!file::exists(filename)) {
    print(stderr, "warning: source file not found: ", filename, "\n");
    return false;
  }

  uint fileNumber = sourceFilenames.size();
  sourceFilenames.append(filename);

  string data = file::read(filename);
  data.transform("\t\r", "  ");

  auto lines = data.split("\n");
  for(uint lineNumber : range(lines.size())) {
    //remove single-line comments
    if(auto position = lines[lineNumber].qfind("//")) {
      lines[lineNumber].resize(position());
    }

    //allow multiple statements per line, separated by ';'
    auto blocks = lines[lineNumber].qsplit(";").strip();
    for(uint blockNumber : range(blocks.size())) {
      string statement = blocks[blockNumber];
      strip(statement);
      if(!statement) continue;

      if(statement.match("include \"?*\"")) {
        statement.trimLeft("include ", 1L).strip();
        source({Location::path(filename), text(statement)});
      } else {
        Instruction instruction;
        instruction.statement = statement;
        instruction.fileNumber = fileNumber;
        instruction.lineNumber = 1 + lineNumber;
        instruction.blockNumber = 1 + blockNumber;
        program.append(instruction);
      }
    }
  }

  return true;
}

auto Bass::define(const string& name, const string& value) -> void {
  defines.insert({name, {}, value});
}

auto Bass::constant(const string& name, const string& value) -> void {
  try {
    constants.insert({name, evaluate(value, Evaluation::Strict)});
  } catch(...) {
  }
}

auto Bass::assemble(bool strict) -> bool {
  this->strict = strict;

  try {
    phase = Phase::Analyze;
    debug("============================== BEGINNING PASS 0 ==============================\n");
    analyze();

    phase = Phase::Query;
    architecture = new Architecture{*this};
    debug("============================== BEGINNING PASS 1 ==============================\n");
    execute();

    for(auto s : orderedUnknowns) debug("Unk(Q): ", s, "\n");

    uint passes = 2;
    if(orderedUnknowns.size()) for(; passes <= 10; passes++) {
      orderedUnknowns.reset();

      debug("\n");
      debug("------------------------------ PREPARING PASS ", passes, " ------------------------------\n");
      debug("investigating constants...\n");

      bool anyChanged = false;
      bool anyUnset = false;
      for(auto& constant : constants) {
        auto name = constant.name;

        if(!constant.held) {
          debug("never set!: ", name, "\n");
          constants.remove(name);
          anyUnset = true;

        } else if(constant.indeterminate) {
          if(constant.unknown) debug("fwdlabel: ", name, "\n");
          debug("unmarking as indeterminate: ", name, "\n");
          constant.indeterminate = false;
          constant.held = false;

        } else if(constant.unknown) {
          debug("DISCOVERED: ", name, "\n");
          constant.unknown = false;
        }

        if(constant.changed && constant.held) anyChanged = true;
        constant.changed = false;
      }

      if(!anyChanged) {
        if(anyUnset) {
          debug("failed to determine a constant, breaking...\n");
          break;
        }

        debug("breaking...\n");
        for(auto& constant : constants) {
          constant.indeterminate = false;
          constant.unknown = false;
          constant.held = true;
        }
        break;
      }

      debug("============================== BEGINNING PASS ", passes, " ==============================\n");
      phase = Phase::Refine;
      architecture = new Architecture{*this};
      execute();
    }

    debug("============================ BEGINNING FINAL PASS ============================\n");
    phase = Phase::Write;
    architecture = new Architecture{*this};
    execute();
  } catch(...) {
    return false;
  }

  return true;
}

//internal

auto Bass::pc() const -> uint {
  return origin + base;
}

auto Bass::seek(uint offset) -> void {
  if(!targetFile) return;
  if(writePhase()) targetFile.seek(offset);
}

auto Bass::track(uint length) -> void {
  if(!tracker.enable) return;
  uint64_t address = targetFile.offset();
  for(auto n : range(length)) {
    if(tracker.addresses.find(address + n)) {
      error("overwrite detected at address 0x", hex(address + n), " [0x", hex(base + address + n), "]");
    }
    tracker.addresses.insert(address + n);
  }
}

auto Bass::write(uint64_t data, uint length) -> void {
  if(writePhase()) {
    if(targetFile) {
      track(length);
      if(endian == Endian::LSB) targetFile.writel(data, length);
      if(endian == Endian::MSB) targetFile.writem(data, length);
    } else if(!isatty(fileno(stdout))) {
      if(endian == Endian::LSB) for(uint n : range(length)) fputc(data >> n * 8, stdout);
      if(endian == Endian::MSB) for(uint n : reverse(range(length))) fputc(data >> n * 8, stdout);
    }
  }
  origin += length;
}

auto Bass::printInstruction() -> void {
  if(activeInstruction) {
    auto& i = *activeInstruction;
    print(stderr, sourceFilenames[i.fileNumber], ":", i.lineNumber, ":", i.blockNumber, ": ", i.statement, "\n");
  }
}

template<typename... P> auto Bass::notice(P&&... p) -> void {
  string s{forward<P>(p)...};
  print(stderr, terminal::color::gray("notice: "), s, "\n");
  printInstruction();
}

template<typename... P> auto Bass::warning(P&&... p) -> void {
  string s{forward<P>(p)...};
  print(stderr, terminal::color::yellow("warning: "), s, "\n");
  if(!strict) {
    printInstruction();
    return;
  }

  printInstructionStack();
  struct BassWarning {};
  throw BassWarning();
}

template<typename... P> auto Bass::error(P&&... p) -> void {
  string s{forward<P>(p)...};
  print(stderr, terminal::color::red("error: "), s, "\n");
  printInstructionStack();

  struct BassError {};
  throw BassError();
}

auto Bass::printInstructionStack() -> void {
  printInstruction();

  for(const auto& frame : reverse(frames)) {
    if(frame.ip > 0 && frame.ip <= program.size()) {
      auto& i = program[frame.ip - 1];
      print(stderr, "   ", sourceFilenames[i.fileNumber], ":", i.lineNumber, ":", i.blockNumber, ": ", i.statement, "\n");
    }
  }
}
