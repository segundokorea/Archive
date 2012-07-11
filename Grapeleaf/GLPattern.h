#import <Foundation/NSObject.h>
#import <Foundation/NSValue.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>
#import <Foundation/NSDictionary.h>
#import <stdlib.h>
#import "Grapeleaf.h"

@interface GLPattern : NSObject <Pattern> {
  unsigned int identificationNumber;
  NSString *description;
  double *inputValues;
  double outputValue;
  unsigned int numberOfInputValues;
}

@property (readonly) unsigned int identificationNumber;
@property (copy, readonly) NSString *description;
@property (readonly) double *inputValues;
@property (readonly) double outputValue;
@property (readonly) unsigned int numberOfInputValues;

+ (NSArray *)patternsWithContentsOfFile:(NSString *)path;
- (id)initWithInputArray:(NSArray *)_inputValues andOutputValue:(NSNumber *)_outputValue;

@end