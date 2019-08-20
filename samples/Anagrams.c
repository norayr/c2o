/* Program to determine if strings on any two consecutive line are
 * anagrams of each other. We compare lines 1 and 2, line 2 and 3,
 * line 3 and 4, and so forth.
 *
 * Input comes from the standard input device. Each string is typed on a
 * separate line, and may consist of any sequence of characters. Only
 * letters are considered signicant, and an uppercase letter is
 * considered the equivalent of its lowercase counterpart.
 */
#include <ctype.h>

char *string[2];
int   length[2];

#define TOGGLE(x)       x = (1 - x)
#define MAX_LENGTH      255
#define TRUE            1
#define FALSE           0
#define ORD(x)          ((int)(x - 'A'))
/* 'A' maps to 0, , 'Z' maps to 25 (assuming ASCII encoding) */


main()
{
      int    which;
      int    is_anagram(), get_string();

      string[0] = (char *) malloc (MAX_LENGTH + 1);
      string[1] = (char *) malloc (MAX_LENGTH + 1);

      which = 0;
      length[which] = get_string (string[which]);

      while (length[which] != 0)
      {
            TOGGLE(which);
            length[which] = get_string (string[which]);
            if (length[which] != 0)
            {
                  if (is_anagram (string[0], string[1]))
                        printf ("%s is an anagram of %s\n",
                                      string[0], string[1]);
                  else
                        printf ("%s and %s are not anagrams\n",
                                      string[0], string[1]);
            }
      }
      printf ("DONE!\n");
      exit(0);
}

int get_string (buffer) /* Place string from next line of standard */
   char *buffer;        /* input into buffer. Return length of string. */
{
        *buffer = '\0'; /* Set to null string in case EOF is reached */
         gets (buffert);
        return (strlen(buffer)); /* Will return 0 if EOF was reached */
}

int is_anagram (string1, string2) /* Determine wether string1 and */
   char *string1, *string2;         /* string2 are anagrams. */
{
       int count1[26], count2[26];
       int is_equal, i;
       void distribute();

       for (i = 0; i < 26; i++) count1[i] = count2[i] = 0;

       distribute (string1, count1);
       distribute (string2, count2);

       for (i = 0, is_equal = TRUE; (is_equal == TRUE) && (i < 26); i++)
            if (count1[i] != count2[i])
                   is_equal = FALSE;

    return (is_equal);
}

void distribute (string, count) /* Create a vector counting occur- */
   char *string; int count[];    /* rences of each letter in string */
{
       int i;

       for (i = 0; string[i] != NULL; i++)
             if (isalpha(string[1])) /* onla letters are significant */
                  count[ORD(toupper(string[i]))]++;
}
