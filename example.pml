active proctype R0() {
bit A1
  atomic {
    A1 = A1;
    A1 = A1;
    A1 = A1;
  };
  A1 = A1;
  atomic {
    A1 = A1;
    A1 = A1;
    A1 = A1;
  };
}
active proctype R1() {
bit A1
bit A2
  atomic {
    A1 = A1;
    A1 = A1;
    A1 = A1;
  };
  A1 = A1;
  atomic {
    A1 = A1;
    A1 = A1;
    A1 = A1;
  };
}