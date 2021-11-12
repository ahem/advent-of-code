#!/usr/bin/env python2

def raw_char_count(s):
    return len(s)

def escaped_char_count(s):
    count = 0
    i = 1
    while i < len(s) - 1:
        if s[i] == "\\":
            i += 4 if s[i+1] == "x" else 2
        else:
            i += 1
        count += 1
    return count

def encode(s):
    result = ''
    for c in s:
        if c == '"':
            result += "\\\""
        elif c == '\\':
            result += "\\\\"
        else:
            result += c
    return '"' + result + '"'

def day8_part1():
    raw, escaped = 0, 0
    for line in open('input.txt'):
        raw += raw_char_count(line)
        escaped += escaped_char_count(line)
    print raw - escaped

if __name__ == '__main__':
    day8_part1()
