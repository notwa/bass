Table::Table(Bass& self, const string& table) : Architecture(self) {
  bitval = 0;
  bitpos = 0;
  parseTable(table);
}

auto Table::assemble(const string& statement) -> bool {
  string s = statement;

  if(s.match("instrument \"*\"")) {
    s.trim("instrument \"", "\"", 1L);
    parseTable(s);
    return true;
  }

  uint pc = Architecture::pc();

  for(auto& opcode : table) {
    if(!tokenize(s, opcode.pattern)) continue;

    string_vector args;
    tokenize(args, s, opcode.pattern);
    if(args.size() != opcode.number.size()) continue;

    bool mismatch = false;
    for(auto& format : opcode.format) {
      if(format.match == Format::Match::Weak) {
        // Do nothing
      } else if(format.type == Format::Type::Absolute) {
        uint bits = bitLength(args[format.argument]);
        if(format.match == Format::Match::Strong) {
          if(bits > opcode.number[format.argument].bits) {
            mismatch = true;
            break;
          }
        } else if(format.match == Format::Match::Exact) {
          if(bits != opcode.number[format.argument].bits) {
            mismatch = true;
            break;
          }
        }
      }
    }
    if(mismatch) continue;

    for(auto& format : opcode.format) {
      switch(format.type) {
        case Format::Type::Static: {
          writeBits(format.data, format.bits);
          break;
        }

        case Format::Type::Absolute: {
          uint data = evaluate(args[format.argument]);
          writeBits(data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::Relative: {
          int data = evaluate(args[format.argument]) - (pc + format.displacement);
          uint bits = opcode.number[format.argument].bits;
          int min = -(1 << (bits - 1)), max = +(1 << (bits - 1)) - 1;
          if(data < min || data > max) {
            error("branch out of bounds: ", data);
          }
          writeBits(data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::Repeat: {
          uint data = evaluate(args[format.argument]);
          for(uint n : range(data)) {
            writeBits(format.data, opcode.number[format.argument].bits);
          }
          break;
        }

        case Format::Type::ShiftRight: {
          uint64_t data = evaluate(args[format.argument]);
          writeBits(data >> format.data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::ShiftLeft: {
          uint64_t data = evaluate(args[format.argument]);
          writeBits(data << format.data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::RelativeShiftRight: {
          int data = evaluate(args[format.argument]) - (pc + format.displacement);
          unsigned bits = opcode.number[format.argument].bits;
          int min = -(1 << (bits - 1)), max = +(1 << (bits - 1)) - 1;
          if(data < min || data > max) error("branch out of bounds");
          bits -= format.data;
          if(endian() == Bass::Endian::LSB) {
            writeBits(data >> format.data, bits);
          } else {
            data >>= format.data;
            writeBits(swapEndian(data, bits), bits);
          }
          break;
        }

        case Format::Type::Negative: {
          unsigned data = evaluate(args[format.argument]);
          writeBits(-data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::NegativeShiftRight: {
          uint64_t data = evaluate(args[format.argument]);
          writeBits(-data >> format.data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::Compliment: {
          unsigned data = evaluate(args[format.argument]);
          writeBits(~data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::ComplimentShiftRight: {
          uint64_t data = evaluate(args[format.argument]);
          writeBits(~data >> format.data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::Decrement: {
          unsigned data = evaluate(args[format.argument]);
          writeBits(--data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::DecrementShiftRight: {
          uint64_t data = evaluate(args[format.argument]);
          writeBits(--data >> format.data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::Increment: {
          unsigned data = evaluate(args[format.argument]);
          writeBits(++data, opcode.number[format.argument].bits);
          break;
        }

        case Format::Type::IncrementShiftRight: {
          uint64_t data = evaluate(args[format.argument]);
          writeBits(++data >> format.data, opcode.number[format.argument].bits);
          break;
        }
      }
    }

    return true;
  }

  return false;
}

auto Table::bitLength(string& text) -> uint {
  auto binLength = [&](const char* p) -> uint {
    uint length = 0;
    while(*p) {
      if(*p == '0' || *p == '1') { p++; length += 1; continue; }
      return 0;
    }
    return length;
  };

  auto hexLength = [&](const char* p) -> uint {
    uint length = 0;
    while(*p) {
      if(*p >= '0' && *p <= '9') { p++; length += 4; continue; }
      if(*p >= 'a' && *p <= 'f') { p++; length += 4; continue; }
      if(*p >= 'A' && *p <= 'F') { p++; length += 4; continue; }
      return 0;
    }
    return length;
  };

  auto decLength = [&](const char *p) -> uint {
    // returns 0 when the input is not a non-negative integer.
    // returns 1 when the input is "0".
    // returns 65 when the input would require more than 64 bits.
    // otherwise, returns the minimum number of bits required
    //            to store an integer, given its decimal encoding.

    struct Meta {
      short offset;
      char elements;
      char bits;
    };

    static const Meta metas[20] = {
      {0,   3, 3},  {3,   3, 6},  {9,   3, 9},  {18,  4, 13},
      {34,  3, 16}, {49,  3, 19}, {67,  4, 23}, {95,  3, 26},
      {119, 3, 29}, {146, 4, 33}, {186, 3, 36}, {219, 3, 39},
      {255, 4, 43}, {307, 3, 46}, {349, 3, 49}, {394, 4, 53},
      {458, 3, 56}, {509, 3, 59}, {563, 4, 63}, {639, 1, 64}
    };

    static const char powers[] =
      "842643216512256128819240962048102465536327681638452428826214413107"
      "283886084194304209715210485766710886433554432167772165368709122684"
      "354561342177288589934592429496729621474836481073741824687194767363"
      "435973836817179869184549755813888274877906944137438953472879609302"
      "220843980465111042199023255552109951162777670368744177664351843720"
      "888321759218604441656294995342131228147497671065614073748835532890"
      "071992547409924503599627370496225179981368524811258999068426247205"
      "759403792793636028797018963968180143985094819845764607523034234882"
      "882303761517117441441151880758558729223372036854775808461168601842"
      "73879042305843009213693952115292150460684697618446744073709551616";

    int i;

    for(i = 0; ; i++) {
      char c = p[i];
      if(c == '\0') break;
      if(c < '0' || c > '9') return 0;
    }
    if(i == 0) return 0;
    if(i >= 21) return 65;

    const int length = i;
    const Meta meta = metas[length - 1];
    const char *start = powers + meta.offset;

    for(i = 0; i < meta.elements; i++) {
      if(strncmp(p, start, length) >= 0) break;
      start += length;
    }
    return meta.bits - i + 1;
  };

  char* p = text.get();
  if(*p == '<') { *p = ' '; return  8; }
  if(*p == '>') { *p = ' '; return 16; }
  if(*p == '^') { *p = ' '; return 24; }
  if(*p == '?') { *p = ' '; return 32; }
  if(*p == ':') { *p = ' '; return 64; }

  uint length = 0;
  if(*p == '%') length = binLength(p + 1);
  if(*p == '$') length = hexLength(p + 1);
  if(*p == '0' && *(p + 1) == 'b') length = binLength(p + 2);
  if(*p == '0' && *(p + 1) == 'x') length = hexLength(p + 2);
  if(*p >= '0' && *p <= '9') length = decLength(p);
  if(length) return length;

  auto data = evaluate(text);
  return data >= 0 ? floor(log2(data)) + 1 : 64;
}

auto Table::writeBits(uint64_t data, uint length) -> void {
  function<uint64_t(unsigned)> setBits = [&](unsigned n) -> uint64_t {
    // Create a bit mask with the n least significant bits set
    return (1 << n) - 1;
  };
  bitval <<= length;
  bitval |= data & setBits(length);
  bitpos += length;

  while(bitpos >= 8) {
    write(bitval);
    bitval >>= 8;
    bitpos -= 8;
  }
}

auto Table::parseTable(const string& text) -> bool {
  auto lines = text.split("\n");
  for(auto& line : lines) {
    if(auto position = line.find("//")) line.resize(position());  //remove comments

    if(line[0] == '#') {
      if(line == "#endian lsb") { setEndian(Bass::Endian::LSB); continue; }
      if(line == "#endian msb") { setEndian(Bass::Endian::MSB); continue; }

      if(auto position = line.find("#include ") ) {
        line.trimLeft("#include ", 1L);
        auto more = readArchitecture(line.strip());
        parseTable(more);
        continue;
      }
      if(auto position = line.find("#directive ") ) {
        parseDirective(line);
      }
    }

    auto part = line.split(";", 1L).strip();
    if(part.size() != 2) continue;

    Opcode opcode;
    assembleTableLHS(opcode, part(0));
    assembleTableRHS(opcode, part(1));
    table.append(opcode);
  }

  return true;
}

// #directive <name> <byte_size>
auto Table::parseDirective(string& line) -> void {
  auto work = line.strip();
  work.trimLeft("#directive ", 1L);

  auto items = work.split(" ");
  if(items.size() != 2) {
    error("Wrong syntax: '",line , "'\n");
  }

  auto& key = items[0];
  key.append(" ");

  uint value = atoi(items[1]);

  for(auto& d : directives().EmitBytes) {
    if(key.equals(d.token)) {
      d.dataLength = value;
      return;
    }
  }

  directives().add(key, value);
}


auto Table::assembleTableLHS(Opcode& opcode, const string& text) -> void {
  uint offset = 0;

  auto length = [&] {
    uint length = 0;
    while(text[offset + length]) {
      char n = text[offset + length];
      if(n == '*') break;
      length++;
    }
    return length;
  };

  while(text[offset]) {
    uint size = length();
    opcode.prefix.append({slice(text, offset, size), size});
    offset += size;

    if(text[offset] != '*') continue;
    uint bits = 10 * (text[offset + 1] - '0');
    bits += text[offset + 2] - '0';
    opcode.number.append({bits});
    offset += 3;
  }

  for(auto& prefix : opcode.prefix) {
    opcode.pattern.append(prefix.text, "*");
  }
  opcode.pattern.trimRight("*", 1L);
  if(opcode.number.size() == opcode.prefix.size()) opcode.pattern.append("*");
}

auto Table::assembleTableRHS(Opcode& opcode, const string& text) -> void {
  uint offset = 0;

  auto list = text.split(" ");
  for(auto& item : list) {
    if(item[0] == '$' && item.length() == 3) {
      Format format = {Format::Type::Static};
      format.data = toHex((const char*)item + 1);
      format.bits = (item.length() - 1) * 4;
      opcode.format.append(format);
    }

    // >>XXa
    else if(item[0] == '>' && item[1] == '>') {
      Format format = {Format::Type::ShiftRight, Format::Match::Weak};
      format.argument = (item[4] >= 'A' && item[4] <= 'Z') ? item[4] + 123 - 'A' - 'a' : item[4] - 'a';
      format.data = (item[2] - '0') * 10 + (item[3] - '0');
      opcode.format.append(format);
    }

    // <<XXa
    else if(item[0] == '<' && item[1] == '<') {
      Format format = {Format::Type::ShiftLeft, Format::Match::Weak};
      format.argument = (item[4] >= 'A' && item[4] <= 'Z') ? item[4] + 123 - 'A' - 'a' : item[4] - 'a';
      format.data = (item[2] - '0') * 10 + (item[3] - '0');
      opcode.format.append(format);
    }

    // +X>>YYa
    else if(item[0] == '+' && item[2] == '>' && item[3] == '>') {
      Format format = {Format::Type::RelativeShiftRight, Format::Match::Weak};
      format.argument = (item[6] >= 'A' && item[6] <= 'Z') ? item[6] + 123 - 'A' - 'a' : item[6] - 'a';
      format.displacement = +(item[1] - '0');
      format.data = (item[4] - '0') * 10 + (item[5] - '0');
      opcode.format.append(format);
    }

    // N>>XXa
    else if(item[0] == 'N' && item[1] == '>' && item[2] == '>') {
      Format format = {Format::Type::NegativeShiftRight, Format::Match::Weak};
      format.argument = (item[5] >= 'A' && item[5] <= 'Z') ? item[5] + 123 - 'A' - 'a' : item[5] - 'a';
      format.data = (item[3] - '0') * 10 + (item[4] - '0');
      opcode.format.append(format);
    }

    // Na
    else if(item[0] == 'N' && item[1] != '>') {
      Format format = {Format::Type::Negative, Format::Match::Weak};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      opcode.format.append(format);
    }

    // C>>XXa
    else if(item[0] == 'C' && item[1] == '>' && item[2] == '>') {
      Format format = {Format::Type::ComplimentShiftRight, Format::Match::Weak};
      format.argument = (item[5] >= 'A' && item[5] <= 'Z') ? item[5] + 123 - 'A' - 'a' : item[5] - 'a';
      format.data = (item[3] - '0') * 10 + (item[4] - '0');
      opcode.format.append(format);
    }

    // Ca
    else if(item[0] == 'C' && item[1] != '>') {
      Format format = {Format::Type::Compliment, Format::Match::Weak};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      opcode.format.append(format);
    }

    // D>>XXa
    else if(item[0] == 'D' && item[1] == '>' && item[2] == '>') {
      Format format = {Format::Type::DecrementShiftRight, Format::Match::Weak};
      format.argument = (item[5] >= 'A' && item[5] <= 'Z') ? item[5] + 123 - 'A' - 'a' : item[5] - 'a';
      format.data = (item[3] - '0') * 10 + (item[4] - '0');
      opcode.format.append(format);
    }

    // Da
    else if(item[0] == 'D' && item[1] != '>') {
      Format format = {Format::Type::Decrement, Format::Match::Weak};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      opcode.format.append(format);
    }

    // I>>XXa
    else if(item[0] == 'I' && item[1] == '>' && item[2] == '>') {
      Format format = {Format::Type::IncrementShiftRight, Format::Match::Weak};
      format.argument = (item[5] >= 'A' && item[5] <= 'Z') ? item[5] + 123 - 'A' - 'a' : item[5] - 'a';
      format.data = (item[3] - '0') * 10 + (item[4] - '0');
      opcode.format.append(format);
    }

    // Ia
    else if(item[0] == 'I' && item[1] != '>') {
      Format format = {Format::Type::Increment, Format::Match::Weak};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      opcode.format.append(format);
    }

    else if(item[0] == '%') {
      Format format = {Format::Type::Static};
      format.data = toBinary((const char*)item + 1);
      format.bits = (item.length() - 1);
      opcode.format.append(format);
    }

    else if(item[0] == '!') {
      Format format = {Format::Type::Absolute, Format::Match::Exact};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      opcode.format.append(format);
    }

    else if(item[0] == '=') {
      Format format = {Format::Type::Absolute, Format::Match::Strong};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      opcode.format.append(format);
    }

    else if(item[0] == '~') {
      Format format = {Format::Type::Absolute, Format::Match::Weak};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      opcode.format.append(format);
    }

    else if(item[0] == '+') {
      Format format = {Format::Type::Relative};
      format.argument = (item[2] >= 'A' && item[2] <= 'Z') ? item[2] + 123 - 'A' - 'a' : item[2] - 'a';
      format.displacement = +(item[1] - '0');
      opcode.format.append(format);
    }

    else if(item[0] == '-') {
      Format format = {Format::Type::Relative};
      format.argument = (item[2] >= 'A' && item[2] <= 'Z') ? item[2] + 123 - 'A' - 'a' : item[2] - 'a';
      format.displacement = -(item[1] - '0');
      opcode.format.append(format);
    }

    else if(item[0] == '*') {
      Format format = {Format::Type::Repeat};
      format.argument = (item[1] >= 'A' && item[1] <= 'Z') ? item[1] + 123 - 'A' - 'a' : item[1] - 'a';
      format.data = toHex((const char*)item + 3);
      opcode.format.append(format);
    }
  }
}

auto Table::swapEndian(uint64_t data, unsigned bits) -> uint64_t {
  int t_data = 0;
  switch((bits - 1) / 8) {
    case 3: { // 4 bytes
      t_data = ((data & 0xFF000000) >> 24) | \
               ((data & 0x00FF0000) >> 8) | \
               ((data & 0x0000FF00) << 8) | \
               ((data & 0x000000FF) << 24);
      break;
    }
    case 2: { // 3 bytes
      t_data = ((data & 0xFF0000) >> 16) | \
               ((data & 0x00FF00)) | \
               ((data & 0x0000FF) << 16);
      break;
    }
    case 1: { // 2 bytes
      t_data = ((data & 0xFF00) >> 8) | \
               ((data & 0x00FF) << 8);
      break;
    }
    case 0: { // byte
      t_data = data;
      break;
    }
    default: {
      error("Invalid number of bits for BassTable::swapEndian");
      break;
    }
  }
  return t_data;
}
