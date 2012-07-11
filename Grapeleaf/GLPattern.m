#import "GLPattern.h"

@implementation GLPattern

static unsigned int ticketNumber = 0;

@synthesize identificationNumber;
@synthesize description;
@synthesize inputValues;
@synthesize numberOfInputValues;
@synthesize outputValue;

+ (NSArray *)patternsWithContentsOfFile:(NSString *)path {
  NSDictionary *patternFile = [NSDictionary dictionaryWithContentsOfFile:path];
  NSMutableArray *patterns  = [[[NSMutableArray alloc] initWithCapacity:[patternFile count]] retain];

  for ( NSString *pattern in patternFile ) {
    [patterns addObject:[[GLPattern alloc] initWithInputArray:[[patternFile objectForKey:pattern] objectForKey:@"Inputs"] andOutputValue:[[patternFile objectForKey:pattern] objectForKey:@"Output"]]];
  }

  return patterns;
}

- (id)initWithInputArray:(NSArray *)_inputValues andOutputValue:(NSNumber *)_outputValue {
  identificationNumber = ticketNumber++;
  description          = @"Generic Pattern";
  inputValues          = (double *) malloc( [_inputValues count] * sizeof( double ) );
  outputValue          = [_outputValue doubleValue];
  numberOfInputValues  = [_inputValues count];

  for ( int i = 0; i < numberOfInputValues; ++i ) {
    inputValues[i] = [[_inputValues objectAtIndex:i] doubleValue];
  }

  return self;
}

@end