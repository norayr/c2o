/*
 *		   IBM RBBS-PC Tulsa, OK
 *		   Switching totally to the "C" Language
 *		   24 Hour operation 300/1200 baud XMODEM
 *		   918-664-8737
 *		   SYSOP LYNN LONG
 *
 * Modified for Zortech C & ANSI display Feb 89 by RPP, SAC @ KnowSoft
 *
 *		    Towers of Hanoi
 *
 * The object of the game is to transfer the disks from
 * the leftmost tower to the rightmost tower obeying the
 * following set of rules:
 *
 * 1) Only the top disk of any tower may be moved at a time
 * 2) At no time must a larger disk be placed on a smaller disk
 *
 */

#define ESC			0x1b

#define MAX_RINGS		7	/* any more and we take forever */

#define POST			0xba	/* vertical double bar		*/
#define POST_BASE		0xca	/* T shape _|_			*/
#define BASE			0xcd	/* horizontal double bar	*/
#define RING			0xdc	/* solid block			*/

#define SCREEN_WIDTH		80	/* number of columns		*/
#define SCREEN_HEIGHT		25	/* number of lines		*/
#define BASE_ROW		15	/* line no. of base of towers	*/
#define RING_WIDTH		((((SCREEN_WIDTH - 2)/3) & 0xfe)-1)
#define LEFT_POST		(RING_WIDTH/2+1)
#define CENTER_POST		(LEFT_POST+RING_WIDTH)
#define RIGHT_POST		(LEFT_POST+2*RING_WIDTH)

#define MOVING_ROW		2
#define POST_HEIGHT		11	/* no. of lines in post		*/

int	top[] = {BASE_ROW-1,BASE_ROW-1,BASE_ROW-1};

int pause;


main()
{
	int nrings;

	printf("The Towers of Hanoi:\n");
	printf("\n");
	printf("This program is setup for use with an ANSI.SYS driver.\n");
	printf("\n");

	do {
		printf("Enter number of rings (between 1 and %d):",
			MAX_RINGS);
		scanf("%d", &nrings);
	} while (nrings < 1 || nrings > MAX_RINGS);
	
	do {
		printf("Speed factor (1=fast to 10=slow):");
		scanf("%d", &pause);
	} while (pause < 1 || pause > 10);

	setup(nrings);
	
	/* move nrings from post 0 to post 2 using post 1 */
	hanoi(nrings, 0, 2, 1);

	curse(0, SCREEN_HEIGHT-2);
}



hanoi(n, a, b, c)
int	n, a, b, c;
{
	if (n == 0)	/* no more rings to move */
		return;

	hanoi(n-1, a, c, b);
	movering(n, a, b);
	hanoi(n-1, c, b, a);
}

setup(n)
int	n;
{
	int	i;
	
	/* ANSI sequence to clear screen: */
	curse(0, 0); outc(ESC); printf("[0J");
	
	/* draw three posts: */
	for(i = MOVING_ROW+2; i < BASE_ROW; ++i) {
		cput(LEFT_POST, i, POST);
		cput(CENTER_POST, i, POST);
		cput(RIGHT_POST, i, POST);
	}
	
	/* draw the base: */
	curse(0, BASE_ROW);
	for(i = 1; i < SCREEN_WIDTH; ++i)
		outc(BASE);

	/* draw the bottoms of the posts: */
	cput(LEFT_POST, BASE_ROW, POST_BASE);
	cput(CENTER_POST, BASE_ROW, POST_BASE);
	cput(RIGHT_POST, BASE_ROW, POST_BASE);

	/* finally, draw in each ring on the left post: */
	for(i = n; i > 0; --i)
		draw(i, LEFT_POST, top[0]--, RING);
}

outc(c)
char	c;
{
	printf("%c", c);
}
	
/* move the cursor to col x, row y */
curse(x, y)
int	x, y;
{
	wait();
	outc(ESC); outc('[');
	printf("%d;%dH", y+1, x+1);
}

/* move the cursor to col x, row y, and output the character ch */
cput(x, y, ch)
char	ch;
int	x, y;
{
	curse(x, y);
	outc(ch);
}


/* draw a 'ring' of character ch, on row y, centred on col centre */
draw(ringno, centre, y, ch)
int	ringno, centre, y;
char	ch;
{
	int	i;
	curse(centre-ringno, y);
	for(i=0; i<ringno; ++i)
		outc(ch);
	curse(centre+1, y);
	for(i=0; i<ringno; ++i)
		outc(ch);
}

/* move a given ring from post 'from' to post 'to' */
movering(ringno, from, to)
int	ringno, from, to;
{
	int	fromc, toc,
		fromy, toy;

	fromc = LEFT_POST + from * RING_WIDTH;
	toc = LEFT_POST + to * RING_WIDTH;
	fromy = ++top[from];
	toy = top[to]--;

	while (fromy != MOVING_ROW) {
	/* lift the ring up off the post: */
		draw(ringno, fromc, fromy, ' ');
		draw(ringno, fromc, --fromy, RING);
	}

	/*
	 *	move the ring over the desired post as follows:
	 *		xxx xxx
	 *		 xx xxx
	 *		 xxxxxx
	 *		 xxx xx
	 *		 xxx xxx
	 */
	if (fromc < toc)
		while (fromc != toc) {
			cput(fromc-ringno, fromy, ' ');
			cput(fromc, fromy, RING);
			cput(fromc+1, fromy, ' ');
			cput(fromc+ringno+1, fromy, RING);
			++fromc;
		}
	else if (fromc > toc)
		while (fromc != toc) {
			cput(fromc+ringno, fromy, ' ');
			cput(fromc, fromy, RING);
			cput(fromc-1, fromy, ' ');
			cput(fromc-ringno-1, fromy, RING);
			--fromc;
		}

	while (fromy != toy) {
	/* now lower the ring onto the new post: */
		draw(ringno, fromc, fromy, ' ');
		draw(ringno, fromc, ++fromy, RING);
	}
}


/* do nothing for a while */
wait()
{
	int i;
	i = 1 << pause;
	while (i)
		i--;
}
