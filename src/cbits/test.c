/*-
 * Written by Austin Seipp, 2012
 * Placed in the Public Domain
 *
 * This is just a simple example of using
 * the portable bsdiff API.
 *
 * Compile with:
 *
 *   $ cc -std=gnu99 -O2 test.c bsdiff.c bspatch.c
 *
 * Usage:
 *
 *   $ ./a.out gen <v1> <v2> <patch>
 *   $ ./a.out app <v1> <patch> <v2>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <err.h>

#include "bspatch.h"
#include "bsdiff.h"

static void
usage(const char* name)
{
  errx(1, "usage:\n\n"
          "Generate patch:"
          "\t$ %s gen <v1> <v2> <patch>\n"
          "Apply patch:"
          "\t$ %s app <v1> <patch> <v2>\n", name, name);
}

void
diff(const char* oldf, const char* newf, const char* patchf)
{
  printf("Generating binary patch between %s and %s\n", oldf, newf);

  printf("Created patch file %s\n", patchf);
  exit(EXIT_SUCCESS);
}
void
patch(const char* inf, const char* patchf, const char* outf)
{
  printf("Applying binary patch %s to %s\n", patchf, inf);

  printf("Successfully applied patch; new file is %s\n", outf);
  exit(EXIT_SUCCESS);
}

int
main(int ac, char* av[])
{
  if (ac != 5) usage(av[0]);

  if (memcmp(av[1], "gen", 3) == 0)
    diff(av[2], av[3], av[4]);

  if (memcmp(av[1], "app", 3) == 0)
    patch(av[2], av[3], av[4]);

  usage(av[0]); /* patch()/diff() don't return */
  return 0;
}
