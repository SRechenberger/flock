# flock
A library to simulate the flocking behavior of programmable autonomous agents.
The agent's desing is based on
[Kilobot: A Low Cost Scalable Robot System for Collective Behavior](https://dash.harvard.edu/bitstream/handle/1/9367001/rubenstein_kilobotlow.pdf?sequence=1),
[A Minimalist Flocking Algorithm for Swarm Robots](https://pdfs.semanticscholar.org/a640/6e3955b1572977addfb2fecb7730262f420e.pdf) and
[Self-Organized Flocking with a Mobile Robot Swarm](http://www.kovan.ceng.metu.edu.tr/pub/pdf/kobot_aamas08.pdf).

# Building and Installing

You can either use this as a library and add it to your projects dependencies (see [stack/faq.md](https://github.com/commercialhaskell/stack/blob/master/doc/faq.md)),
or you can clone this directory and either reimplement the `behavior` function in [src/Main.hs](https://github.com/SRechenberger/flock/blob/master/src/Main.hs), or add a new source file, from which a new executable should be build; then build with 

    stack build 

and run with, e.g.

    stack exec flock

# Controlling a simulation
With a simulation running, you may *add* new *agents* by clicking **right**, where you want to have one and an *obstacle* by clicking **left**.
If you want to *pause* or *unpause* the simulation, press **space**, and **r** to reset everything.
