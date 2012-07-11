#import "GLSynapse.h"

@implementation GLSynapse

static unsigned int ticketNumber = 0;

@synthesize identificationNumber;
@synthesize description;
@synthesize weightValue;
@synthesize inputNeuron;
@synthesize outputNeuron;

- (id)init { return [self initWithDescription:@"Generic Synapse"]; }

- (id)initWithDescription:(NSString *)_description {
  return [self initWithDescription:_description andInputNeuron:nil andOutputNeuron:nil];
}

- (id)initWithDescription:(NSString *)_description andInputNeuron:(id <Neuron>)_inputNeuron andOutputNeuron:(id <Neuron>)_outputNeuron {
  identificationNumber = ticketNumber++;
  description          = _description;
  weightValue          = arc4random() % 100;

  // Squash weightValue into [-1, 1]
  arc4random_stir();
  weightValue /= arc4random() % 100;
  if ( weightValue > 1 ) {
    weightValue = weightValue - (int)weightValue;
  }
  arc4random_stir();
  if ( arc4random() % 100 >= 50 ) {
    weightValue *= -1;
  }

  if ( isinf( weightValue ) ) {
    weightValue = 0.000;
  }

  // Connect with input and output neurons
  inputNeuron  = _inputNeuron;
  outputNeuron = _outputNeuron;
  [[inputNeuron outputSynapses] addObject:self];
  [[outputNeuron inputSynapses] addObject:self];

  return self;
}

- (double)inputNeuronValue { return inputNeuron.value; }
- (void)setInputNeuronValue:(double)newValue { inputNeuron.value = newValue; }
- (double)inputNeuronErrorValue { return inputNeuron.errorValue; }
- (void)setInputNeuronErrorValue:(double)newErrorValue { inputNeuron.errorValue = newErrorValue; }

- (double)outputNeuronValue { return outputNeuron.value; }
- (void)setOutputNeuronValue:(double)newValue { outputNeuron.value = newValue; }
- (double)outputNeuronErrorValue { return outputNeuron.errorValue; }
- (void)setOutputNeuronErrorValue:(double)newErrorValue { outputNeuron.errorValue = newErrorValue; }

- (double)weightedInputNeuronValue { return self.inputNeuronValue * weightValue; }
- (double)weightedOutputNeuronValue { return self.outputNeuronValue * weightValue; }
- (double)weightedInputNeuronErrorValue { return self.inputNeuronErrorValue * weightValue; }
- (double)weightedOutputNeuronErrorValue { return self.outputNeuronErrorValue * weightValue; }

@end