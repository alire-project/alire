#!/usr/bin/env python3
# Generate the man page from the Alire builtin help
#
# It assumes the alr command is available in 'bin/alr'
# Man pages are produced in doc/man1.  Run with:
#
#  python3 scripts/python/generate-man.py
#
# To check a man page, setup:
#
#  export MANPATH=`pwd`/doc
#
# and then type `man alr` or `man alr-with`

import os
import re
import subprocess
import enum

class State(enum.Enum):
    In_Unkown = 0
    In_Summary = 1
    In_Synopsis = 2
    In_Options = 3
    In_Description = 4
    In_Other = 5

class Generator:
    def __init__(self):
        self.state = State.In_Unkown
        self.name = "alr"
        self.summary = ''
        self.date = 'Aug 3, 2022'
        self.version = "1.2"
        self.previous_line_empty = False
        self.preformat_mode = False
        self.general_commands = ["config", "printenv", "toolchain", "version"]
        self.index_commands = ["get", "index", "init", "pin", "search", "show", "update", "with"]
        self.build_commands = ["action", "build", "clean", "dev", "edit", "run", "test", "exec"]
        self.publish_commands = ["publish"]

    def emit_header(self, outfile, command):
        title_upper = command.upper()
        outfile.write(f".TH {title_upper} 1 \"{self.date}\" \"Alire {self.version}\" \"Alire manual\"\n")
        outfile.write(".nh\n")
        outfile.write(".ad l\n")
        outfile.write(".SH NAME\n")
        outfile.write(f"{command} \\- {self.summary}\n")
        outfile.write(".\\\"\n")
        outfile.write(".SH SYNOPSIS\n")
        outfile.write(".sp\n")

    def emit_footer(self, outfile, name, commands):
        outfile.write("\n.SH SEE ALSO\n")
        if name != "":
            outfile.write("\\fIalr(1)\\fR, ")

        for command in commands:
            if command != name:
                outfile.write(f"\\fIalr-{command}(1)\\fR, ")

        outfile.write("\\fIgprbuild(1)\\fR")
        outfile.write("\n.SH AUTHOR\n")
        outfile.write("Generated with generate-man from Alire execution\n")

    def emit_option(self, outfile, line):
        items = line.split()
        if len(items) == 0:
            return

        outfile.write(".TP 5\n")
        first = True
        check_description = True
        for item in items:
            item = re.sub(r'=([^\)]*)', r'=\\fI\1\\fP', item)
            if first:
                item = re.sub(r'(\[.*\])', r'\\fI\1\\fP', item)
                outfile.write(item)
                first = False
            elif check_description and item[0] == '(':
                item = re.sub(r'[\(\)]', r'', item)
                outfile.write(', ')
                outfile.write(item)
            elif check_description and item.istitle:
                check_description = False
                outfile.write("\n")
                outfile.write(item)
            else:
                outfile.write(' ')
                outfile.write(item)
        outfile.write("\n")

    def emit_synopsis(self, outfile, line):
        line = re.sub(r'\-', r'\\-', line)
        items = line.split()
        if len(items) == 0:
            return
        is_cmd = True
        need_separator = False
        outfile.write("\\fI")
        for item in items:
            if is_cmd and item[0] == '[':
                outfile.write("\\fP")
                is_cmd = False
            if need_separator:
                outfile.write(" ")
            outfile.write(item)
            need_separator = True
        outfile.write("\n")

    def emit_description(self, outfile, line):
        if line == "":
            if not self.previous_line_empty:
                outfile.write(".PP\n")
            self.previous_line_empty = True
        else:
            # Replace "-P xxx" by \fI-P xxx\fP
            line = re.sub(r'\"(\-[^\"]*)\"', r'\\fI\1\\fP', line)
            line = re.sub(r'--([a-zA-Z_]*)', r'\\fI--\1\\fP', line)
            if re.match(r'^\* .*:', line):
                line = re.sub(r'^\* ', r'', line)
                line = re.sub(r':$', r'', line)
                outfile.write(f".SS {line}\n")

            elif re.match(r'^-[ ]+[a-z]*.*\[.*\]', line):
                line = re.sub(r'^-[ ]*', r'', line)
                outfile.write(f".SS {line}\n")
                
            elif re.match(r'^[A-Z]: ', line) or re.match(r'^crate.*[ \t][A-Z].*', line):
                if self.preformat_mode:
                    outfile.write(".br\n")
                else:
                    outfile.write(".PP\n")
                self.preformat_mode = True
                outfile.write(f"{line}\n")

            elif self.preformat_mode:
                self.preformat_mode = False
                outfile.write(".PP\n")
                outfile.write(f"{line}\n")

            else:
                outfile.write(f"{line}\n")

    def emit(self, outfile, line):
        if line == "SUMMARY":
            self.state = State.In_Summary

        elif line == "USAGE":
            self.emit_header(outfile, "alr-" + self.name)
            self.state = State.In_Synopsis

        elif line == "ARGUMENTS" or line == "OPTIONS":
            outfile.write(".\\\"\n")
            outfile.write(".SH OPTIONS\n")
            self.state = State.In_Options

        elif line == "GLOBAL OPTIONS":
            outfile.write(".\\\"\n")
            outfile.write(".SH GLOBAL OPTIONS\n")
            self.state = State.In_Options

        elif line == "DESCRIPTION":
            outfile.write(".\\\"\n")
            outfile.write(".SH DESCRIPTION\n")
            self.state = State.In_Description

        elif line == "TOPICS" or line == "ALIASES" or line == "COMMANDS":
            outfile.write(".\\\"\n")
            outfile.write(f".SH {line}\n")
            self.state = State.In_Other

        elif self.state == State.In_Summary:
            self.summary = self.summary + line

        elif self.state == State.In_Description:
            self.emit_description(outfile, line)

        elif self.state == State.In_Options:
            self.emit_option(outfile, line)

        elif self.state == State.In_Synopsis:
            self.emit_synopsis(outfile, line)

        else:
            outfile.write(f"{line}\n")

    def generate(self, name, commands):
        """
        Generate an alire man page for a specific command
        The 'bin/alr help {name}' command is executed and parsed to re-format following the man format.
        """
        self.name = name
        self.summary = ''
        self.state = State.In_Unkown
        self.previous_line_empty = False
        self.path = "doc/man1/alr-" + name + ".1"
        with open(self.path, "w") as outfile:
            with subprocess.Popen(["bin/alr", "help", name], stdout=subprocess.PIPE) as proc:
                while True:
                    status = proc.poll()
                    line = proc.stdout.readline()
                    self.emit(outfile, line.decode('utf-8').strip())
                    if status is not None:
                        break

            self.emit_footer(outfile, name, commands)

    def generate_commands(self):
        for name in self.general_commands:
            self.generate(name, self.general_commands)

        for name in self.index_commands:
            self.generate(name, self.index_commands)

        for name in self.build_commands:
            self.generate(name, self.build_commands)

        for name in self.publish_commands:
            self.generate(name, self.build_commands)

    def generate_main(self):
        """
        Generate the alire main man page
        """
        self.path = "doc/man1/alr.1"
        with open(self.path, "w") as outfile:
            self.summary = "Ada Library Repository"
            self.emit_header(outfile, "alr")
            outfile.write(f"\\fIalr\\fP [global options] <command> [command options]] [<arguments>]\n")
            outfile.write(f".SH DESCRIPTION\n")
            with open("doc/introduction.md", "r") as intro:
                while True:
                    line = intro.readline()
                    if not line:
                        break
                    if re.match(r'# Introduction', line):
                        continue
                    line = re.sub(r'`([a-zA-Z]*)`', r'\\fI\1\\fR', line)
                    outfile.write(line)

            # run alr help aliases to print the aliases section
            with subprocess.Popen(["bin/alr", "help", "aliases"], stdout=subprocess.PIPE) as proc:
                line = proc.stdout.readline()
                line = line.decode('utf-8').strip()
                outfile.write(f".SS {line}\n")
                while True:
                    status = proc.poll()
                    line = proc.stdout.readline()
                    self.emit(outfile, line.decode('utf-8').strip())
                    if status is not None:
                        break

            with subprocess.Popen(["bin/alr", "help", "identifiers"], stdout=subprocess.PIPE) as proc:
                line = proc.stdout.readline()
                line = line.decode('utf-8').strip()
                outfile.write(f".SS {line}\n")
                while True:
                    status = proc.poll()
                    line = proc.stdout.readline()
                    self.emit(outfile, line.decode('utf-8').strip())
                    if status is not None:
                        break

            with subprocess.Popen(["bin/alr", "help", "toolchains"], stdout=subprocess.PIPE) as proc:
                line = proc.stdout.readline()
                line = line.decode('utf-8').strip()
                outfile.write(f".SS {line}\n")
                while True:
                    status = proc.poll()
                    line = proc.stdout.readline()
                    self.emit(outfile, line.decode('utf-8').strip())
                    if status is not None:
                        break

            self.emit_footer(outfile, "", self.index_commands)

generator = Generator()
generator.generate_main()
generator.generate_commands()

