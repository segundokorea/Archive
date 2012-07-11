#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <stdlib.h>
#import <math.h>
#import "Grapeleaf.h"

@interface GLSynapse : NSObject <Synapse> {
  NSUInteger identificationNumber;
  NSString *description;
  double weightValue;
  double deltaValue;
  id <Neuron> inputNeuron;
  id <Neuron> outputNeuron;
}

@property (readonly) NSUInteger identificationNumber;
@property (copy, readonly) NSString *description;
@property (assign) double weightValue;
@property (assign) id <Neuron> inputNeuron;
@property (assign) id <Neuron> outputNeuron;

- (id)init;
- (id)initWithDescription:(NSString *)_description;
- (id)initWithDescription:(NSString *)_description andInputNeuron:(id <Neuron>)_inputNeuron andOutputNeuron:(id <Neuron>)_outputNeuron;

- (double)inputNeuronValue;
- (void)setInputNeuronValue:(double)newValue;
- (double)inputNeuronErrorValue;
- (void)setInputNeuronErrorValue:(double)newErrorValue;

- (double)outputNeuronValue;
- (void)setOutputNeuronValue:(double)newValue;
- (double)outputNeuronErrorValue;
- (void)setOutputNeuronErrorValue:(double)newErrorValue;

- (double)weightedInputNeuronValue;
- (double)weightedOutputNeuronValue;
- (double)weightedInputNeuronErrorValue;
- (double)weightedOutputNeuronErrorValue;

@end