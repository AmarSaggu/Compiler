#include "Timer.hpp"

#include <iostream>
#include <functional>
#include <cstdint>

#define RUNS 100

double profile(std::function<void()> fun);

int c()
{
	std::cout << "Testing C\n";
	system("gcc prog.c -o prog.c.run");
	
	int res;

	double total = 0.0;
	
	for (int i = 0; i < RUNS; i++) {
		double time = profile([&]() {
			res = system("./prog.c.run");
		});

		total += time;
	}

	total /= RUNS;
	
	std::cout << "C executed in " << total << " second(s)\n";
	//std::cout << "Returns the result " << (int) res << "\n" << std::endl;

	return res;
}

int yip()
{
	std::cout << "Testing Yip (interpreted)\n";
	system("../main.native prog.yip > prog.yip.run");
	
	int res;
	
	double total = 0.0;

	for (int i = 0; i < RUNS; i++) {
		double time = profile([&]() {
			res = system("lli prog.yip.run");
		});

		total += time;
	}

	total /= RUNS;
	
	std::cout << "Yip (interpreted) executed in " << total << " second(s)\n";
	//std::cout << "Returns the result " << (int) res << "\n" << std::endl;

	return res;
}

int yipc()
{
	std::cout << "Testing Yip (compiled)\n";
	system("../main.native prog.yip > prog.yip.run");

	system("llc prog.yip.run -o prog.yip.s");
	system("gcc prog.yip.s -o prog.yip.out");

	int res;

	double total = 0.0;

	for (int i = 0; i < RUNS; i++) {
		double time = profile([&]() {
			res = system("./prog.yip.out");
		});

		total += time;
	}

	total /= RUNS;
	
	std::cout << "Yip (compiled) executed in " << total << " second(s)\n";
	//std::cout << "Returns the result " << res << "\n" << std::endl;

	return res;
}
int main(int argc, char **argv)
{
	std::string prog = "prog"; //std::string(argv[1]);
	system(("../bench.native " + prog).c_str());

	c();
	yip();
	yipc();

	return 0;
}

double profile(std::function<void()> fun)
{
	Timer timer;
	timer.Start();

	fun();

	timer.Stop();

	return timer.GetElapsedTime();
}
