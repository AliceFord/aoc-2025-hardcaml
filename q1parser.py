out = ""
while (line := input()) != "":
    d = line[0]
    n = int(line[1:])
    if d == "R":
        out += f"({n}, 1); "
    else:
        out += f"({n}, 0); "

print(out)