#import "GLNeuron.h"

@implementation GLNeuron

static NSUInteger ticketNumber = 0;

@synthesize transferFunction;
@synthesize transferDerivative;
@synthesize identificationNumber;
@synthesize description;
@synthesize value;
@synthesize errorValue;
@synthesize momentum;
@synthesize learningRate;
@synthesize inputSynapses;
@synthesize outputSynapses;

- (id)init {
  return [self initWithDescription:@"Generic Neuron"];
}

- (id)initWithDescription:(NSString *)_description {
  return [self initWithDescription:_description andLearningRate:0 andMomentum:0];
}

- (id)initWithDescription:(NSString *)_description andLearningRate:(double)_learningRate andMomentum:(double)_momentum {
  identificationNumber = ticketNumber++;
  description          = _description;
  value                = 0.000;
  errorValue           = 0.000;
  momentum             = _momentum;
  learningRate         = _learningRate;
  transferFunction     = ^(double x) {
    // Sigmoid function, S(x)
    // Used for it's easy representation and derivative
    return 1 / ( 1 + exp( -1 * x ) );
  };

  transferDerivative = ^(double x) {
    // dS(x)/dx = S(x) * [ 1 - S(x) ]
    return x * ( 1 - x );
  };

  inputSynapses  = [NSMutableArray array];
  outputSynapses = [NSMutableArray array];

  return self;
}

- (double)computeTransferFunctionWithValue:(double)valueToTransform {
  // Remember, the tranfer function is sigmoidal
  return transferFunction( valueToTransform );
}

- (double)computeTransferDerivativeWithValue:(double)valueToTransform {
  return transferDerivative( valueToTransform );
}

- (void)forwardPropagate {
  // To compute the output for a neuron:
  // 1. Compute the weighted sum of all inputs
  // 2. Pass sum through transfer function
  double sumOfWeightedInputs = 0.000;
  for ( int i = 0; i < [inputSynapses count]; ++i ) {
    sumOfWeightedInputs += [[inputSynapses objectAtIndex:i] weightedInputNeuronValue];
  }
  value = [self computeTransferFunctionWithValue:sumOfWeightedInputs];

  NSLog( @"[%@ forwardPropagate] (%i) value => %f", [self class], identificationNumber, value ); // DEBUG
}

- (void)computeErrorValueWithExpectedValue:(double)expectedValue {
  // Compute the errorValue for output neurons
  errorValue = ( expectedValue - self.value ) * [self computeTransferDerivativeWithValue:value];

  NSLog( @"[%@ computeErrorValueWithExpectedValue:%f] (%i) errorValue => %f", [self class], identificationNumber, expectedValue, errorValue ); // DEBUG
}

- (void)backwardPropagate {
  // Compute the errorValue for hidden neurons
  // 1. Get the weighted sum of its output neurons
  // 2. Multiply by its own slope
  // Note: Combines the error and derivative steps
  double sumOfWeightedErrorValues = 0.000;
  for ( int i = 0; i < [outputSynapses count]; ++i ) {
    sumOfWeightedErrorValues += [[outputSynapses objectAtIndex:i] weightedOutputNeuronErrorValue];
  }
  errorValue = [self computeTransferDerivativeWithValue:value] * sumOfWeightedErrorValues;

  NSLog( @"[%@ computeErrorWithOutputSynapses] (%i) errorValue => %f", [self class], identificationNumber, errorValue ); // DEBUG
}

- (void)updateWeightsForInputNeurons {
  // Update weights based on errorValues
  for ( int i = 0; i < [inputSynapses count]; ++i ) {
    [[inputSynapses objectAtIndex:i] setWeightValue:learningRate * errorValue * [[inputSynapses objectAtIndex:i] inputNeuronValue]]; // [[inputSynapses objectAtIndex:i] inputNeuronValue];

    NSLog( @"[%@ backwardPropagate] (%i) weightValue => %f", [self class], identificationNumber, [[inputSynapses objectAtIndex:i] weightValue] ); // DEBUG
  }
}

@end