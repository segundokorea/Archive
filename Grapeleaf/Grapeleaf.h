#import <Foundation/NSArray.h>

@protocol Pattern

@property (readonly) unsigned int identificationNumber;
@property (copy, readonly) NSString *description;
@property (readonly) double *inputValues;
@property (readonly) double outputValue;
@property (readonly) unsigned int numberOfInputValues;

- (id)initWithInputArray:(NSArray *)_inputValues andOutputValue:(NSNumber *)_outputValue;

@optional
+ (NSArray *)patternsWithContentsOfFile:(NSString *)path;

@end


@protocol Synapse

@property (readonly) NSUInteger identificationNumber;
@property (copy, readonly) NSString *description;
@property (assign) double weightValue;

- (id)initWithDescription:(NSString *)_description;

- (double)weightedInputNeuronValue;
- (double)weightedOutputNeuronValue;
- (double)weightedInputNeuronErrorValue;
- (double)weightedOutputNeuronErrorValue;

@optional
- (id)init;

- (double)inputNeuronValue;
- (void)setInputNeuronValue:(double)newValue;
- (double)inputNeuronErrorValue;
- (void)setInputNeuronErrorValue:(double)newErrorValue;

- (double)outputNeuronValue;
- (void)setOutputNeuronValue:(double)newValue;
- (double)outputNeuronErrorValue;
- (void)setOutputNeuronErrorValue:(double)newErrorValue;

@end


@protocol Neuron

@property (readonly) NSUInteger identificationNumber;
@property (copy, readonly) NSString *description;
@property (assign) double value;
@property (assign) double errorValue;
@property (assign) double momentum;
@property (assign) double learningRate;

- (id)initWithDescription:(NSString *)_description;

@optional
@property (assign) NSMutableArray *inputSynapses;
@property (assign) NSMutableArray *outputSynapses;
@property (copy) double (^transferFunction)(double x);
@property (copy) double (^transferDerivative)(double x);

- (id)init;
- (void)forwardPropagate;
- (void)computeErrorValueWithExpectedValue:(double)expectedValue;
- (double)computeTransferDerivativeWithValue:(double)valueToTransform;
- (void)computeErrorWithOutputSynapses;
- (void)backwardPropagate;
- (void)updateWeightsForInputNeurons;

@end