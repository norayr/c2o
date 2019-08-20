/*
	Sieve of Erastothenes
*/

#define UPTO		8192
#define ITERATIONS	10
#define YES		1
#define NO		0

int	isprime[UPTO];

main()
{
	int	i, j,
		iterate;
	int	count;

	printf("Start...\7\n");
	for (iterate = 0; iterate < ITERATIONS; iterate++) {
		count = 0;
		
		/* initialise array to 'all prime' */
		for (i = 0; i < UPTO; isprime[i++] = YES);

		for (i = 2; i < UPTO; i++)
			if (isprime[i]) {
				count++;
				/* 'cross out' all multiples of i: */
				for (j = 2*i; j < UPTO; j += i)
					isprime[j] = NO;
			}
	}
	printf("Finish...\7 Found %d primes.\n", count);
}
 
 
