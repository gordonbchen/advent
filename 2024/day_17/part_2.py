import sys
import re


def read_file(filename: str) -> tuple[list[int], list[int]]:
    with open(filename, "r") as f:
        data = f.read()

    register_str, program_str = data.split("\n\n")

    num_pattern = re.compile(r"(\d+)")

    registers = [int(i) for i in num_pattern.findall(register_str)]
    program = [int(i) for i in num_pattern.findall(program_str)]

    return registers, program

def get_comb(operand: int, a: int, b: int, c: int):
    if operand <= 3:
        return operand

    registers = (a, b, c)
    return registers[operand - 4]

def run(a: int, b: int, c: int, program: list[int]) -> list[int]:
    i = 0
    output = []

    while (i + 1) < len(program):
        opcode = program[i]
        operand = program[i + 1]

        match opcode:
            case 0:
                # * 2 is a left bit shift, // 2 is a right bit shift.
                a = a >> get_comb(operand, a, b, c)
            case 1:
                b = b ^ operand
            case 2:
                b = get_comb(operand, a, b, c) % 8
            case 3:
                i = operand - 2 if a != 0 else i
            case 4:
                b = b ^ c
            case 5:
                output.append(get_comb(operand, a, b, c) % 8)
            case 6:
                b = a >> get_comb(operand, a, b, c)
            case 7:
                c = a >> get_comb(operand, a, b, c)

        i += 2

    return output


def search(a: int, b: int, c: int, program: list[int], i: int):
    output = run(a, b, c, program)
    if output == program:
        return a
    
    # If the output matches the end of the program,
    # multiply by 8 to get another output from the program,
    # and check nearby nums that will // 8 to the same value. 
    elif (output == program[-i:]) or (i == 0):
        for n in range(8):
            new_a = (8 * a) + n
            if sol := search(new_a, b, c, program, i + 1):
                return sol

    return None


if __name__ == "__main__":
    (a, b, c), program = read_file(sys.argv[1])
    print(a, b, c)
    print(program)

    sol = search(0, b, c, program, 0)
    print(f"\nSolution: {sol}")
    print(run(sol, b, c, program))
