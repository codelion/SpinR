bit FORK0 = 0
bit FORK1 = 0
active proctype PHIL1() {
  atomic {
    ((FORK1 == 0));
    FORK1 = 1;
  };
  atomic {
    ((FORK0 == 0));
    FORK0 = 1;
  };
  FORK1 = 0;
  FORK0 = 0;
}
active proctype PHIL0() {
  atomic {
    ((FORK0 == 0));
    FORK0 = 1;
  };
  atomic {
    ((FORK1 == 0));
    FORK1 = 1;
  };
  FORK0 = 0;
  FORK1 = 0;
}