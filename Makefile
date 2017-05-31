docs: lib/Flock.hs lib/Simulation.hs
	stack haddock --no-haddock-deps
	cp -r .stack-work/install/x86_64-linux/nightly-2017-05-14/8.0.2/doc/flocking-0.1.0.0/* docs
