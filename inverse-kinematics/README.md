# Interactive Inverse Kinematics Knicknack

I wanted to simulate a double pendulum in Elm and I wanted it to be interactive.
Specifically, I wanted the user to be able to pick a starting location,
which was easy enough. The problem was I wanted to generalize this to more than
just a double pendulum, and setting to location of *n* rods in a plane
such that they reach a given point is harder.

I knew this was something I can read about thanks to [Vi Hart][],
and after some googling I figured out this problem is called Inverse Kinematics
and, after some more googling, I found [a website about FABRIK][FABRIK], which is
a very neat algorithm that can solve IK problems.
I implemented it in Elm.

[Vi Hart]: https://www.youtube.com/watch?v=56kLZ6s1rTQ
[FABRIK]: http://www.andreasaristidou.com/FABRIK.html