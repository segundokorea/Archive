// Hailstone.c
// Updated 2010-01-28 by Sean Clemmer
//
#include <stdio.h>
#include <stdlib.h>

int main( int argc, char **argv ) {
  char *usage = "USAGE: Hailstone NATRUAL_NUMBER\n";

  // Make sure we have one argument
  if ( argc < 2 ) {
    printf( "ERROR: No NATURAL_NUMBER provided\n" );
    printf( "%s", usage );
    return 1;
  } else if ( argc > 2 ){
    printf( "ERROR: Too many parameters\n" );
    printf( "%s", usage );
    return 1;
  }

  // Make sure that one argument is a natural number
  // That is, the integer we get is greater than zero
  // And that string we were passed didn't start with minus
  long unsigned element = atoi( argv[1] );
  if ( element <= 0 || argv[1][0] == '-') {
    printf( "ERROR: Not a valid NATURAL_NUMBER\n" );
    printf( "%s", usage );
    return 1;
  }

  // The Real Magic
  printf( "%lu ", element );
  do {
    if ( element % 2 == 0 ) {
      element = element / 2;
    } else {
      element = 3 * element + 1;
    }
    printf( "%lu ", element );
  } while( element != 1 );

  printf( "\n" );
  return 0;
}