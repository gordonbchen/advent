from __future__ import annotations

import sys
import re


class Program:
    def __init__(self, registers: list[int], program: list[int]) -> None:
        self.register_a, self.register_b, self.register_c = registers
        self.program = program

    @staticmethod
    def from_file(filename: str) -> Program:
        with open(filename, "r") as f:
            data = f.read()

        register_str, program_str = data.split("\n\n")

        num_pattern = re.compile(r"(\d+)")

        registers = [int(i) for i in num_pattern.findall(register_str)]
        program = [int(i) for i in num_pattern.findall(program_str)]

        return Program(registers, program)

    def get_combo_operand(self, operand: int) -> int:
        if operand <= 3:
            return operand
        elif operand <= 6:
            registers = [self.register_a, self.register_b, self.register_c]
            return registers[operand - 4]
        raise ValueError(f"Bad operand: {operand}")
    
    def divide_op(self, operand: int) -> int:
        denom = 2 ** self.get_combo_operand(operand)
        return self.register_a // denom

    def run(self) -> list[int]:
        instruct_ptr = 0
        outputs = []

        while (instruct_ptr + 1) < len(self.program):
            opcode = self.program[instruct_ptr]
            operand = self.program[instruct_ptr + 1]

            match opcode:
                case 0:
                    self.register_a = self.divide_op(operand)
                case 1:
                    self.register_b = self.register_b ^ operand
                case 2:
                    self.register_b = self.get_combo_operand(operand) % 8
                case 3:
                    if self.register_a != 0:
                        instruct_ptr = operand
                        instruct_ptr -= 2
                case 4:
                    self.register_b = self.register_b ^ self.register_c
                case 5:
                    outputs.append(self.get_combo_operand(operand) % 8)
                case 6:
                    self.register_b = self.divide_op(operand)
                case 7:
                    self.register_c = self.divide_op(operand)

            instruct_ptr += 2

        return outputs


if __name__ == "__main__":
    program = Program.from_file(sys.argv[1])
    outputs = program.run()
    print(f"Output: {",".join([str(i) for i in outputs])}")