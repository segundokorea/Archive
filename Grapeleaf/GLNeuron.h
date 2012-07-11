#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>
#import <math.h>
#import "Grapeleaf.h"

@interface GLNeuron : NSObject <Neuron> {
  double (^transferFunction)(double x);
  double (^transferDerivative)(double x);
  NSUInteger identificationNumber;
  NSString *description;
  double value;
  double errorValue;
  double momentum;
  double learningRate;
  NSMutableArray *inputSynapses;
  NSMutableArray *outputSynapses;
}

@property (copy) double (^transferFunction)(double x);
@property (copy) double (^transferDerivative)(double x);
@property (readonly) NSUInteger identificationNumber;
@property (copy, readonly) NSString *description;
@property (assign) double value;
@property (assign) double errorValue;
@property (assign) double momentum;
@property (assign) double learningRate;
@property (assign) NSMutableArray *inputSynapses;
@property (assign) NSMutableArray *outputSynapses;

- (id)init;
- (id)initWithDescription:(NSString *)_description;
- (id)initWithDescription:(NSString *)_description andLearningRate:(double)_learningRate andMomentum:(double)_momentum;
- (double)computeTransferFunctionWithValue:(double)valueToTransform;
- (double)computeTransferDerivativeWithValue:(double)valueToTransform;
- (void)forwardPropagate;
- (void)computeErrorValueWithExpectedValue:(double)expectedValue;
- (void)backwardPropagate;
- (void)updateWeightsForInputNeurons;

@end