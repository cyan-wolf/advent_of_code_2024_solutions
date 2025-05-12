import sys

class DoorKeyPad:
    def __init__(self):
        self.buttons = [
            "789",
            "456",
            " 0A",
        ]
        self.pos = (2, 2)

    def get_value(self):
        if self.pos_over_button():
            (r, c) = self.pos
            return self.buttons[r][c]
        else:
            return None

    def pos_over_button(self) -> bool:
        (r, c) = self.pos
        return not (r < 0 or r > 2 or c < 0 or c > 2 or self.pos == (2, 0))

    def press(self, inp: str) -> bool:
        if inp == 'A' and self.pos == (2, 2):
            return True
        elif inp == 'A':
            return False
        
        displacement = None

        if inp == '^':
            displacement = (-1, 0)
        elif inp == 'v':
            displacement = (1, 0)
        elif inp == '<':
            displacement = (0, -1)
        elif inp == '>':
            displacement = (0, 1)

        self.pos = (self.pos[0] + displacement[0], 
                    self.pos[1] + displacement[1])
        
        return self.pos_over_button()


class RobotKeyPad:
    # TODO
    pass


def main():
    filename = sys.argv[1]

    with open(filename, "r") as f:
        codes = [line.strip() for line in f.readlines()]

    print(codes)

    # TESTING:
    # dkp = DoorKeyPad()
    # dkp.press('^')
    # dkp.press('<')
    # print(dkp.get_value())
    #print(dkp.press('A'))


if __name__ == "__main__":
    main()
