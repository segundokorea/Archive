#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>
#import <Foundation/NSDictionary.h>

#import "GLSynapse.h"
#import "GLNeuron.h"
#import "GLPattern.h"


int main( int argc, char *argv[] ) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];


  // Initialize all neurons and synapses
  GLNeuron *Neurons[6];
  GLSynapse *Synapses[6];

  Neurons[0] = [[[GLNeuron alloc] initWithDescription:@"Input Neuron"] autorelease];
  Neurons[1] = [[[GLNeuron alloc] initWithDescription:@"Input Neuron"] autorelease];
  Neurons[2] = [[[GLNeuron alloc] initWithDescription:@"Hidden Neuron" andLearningRate:0.450 andMomentum:0.900] autorelease];
  Neurons[3] = [[[GLNeuron alloc] initWithDescription:@"Hidden Neuron" andLearningRate:0.450 andMomentum:0.900] autorelease];
  Neurons[4] = [[[GLNeuron alloc] initWithDescription:@"Hidden Neuron" andLearningRate:0.450 andMomentum:0.900] autorelease];
  Neurons[5] = [[[GLNeuron alloc] initWithDescription:@"Output Neuron" andLearningRate:0.450 andMomentum:0.900] autorelease];


  // Connect the network
  int k = 0; // Current synapse
  for ( int i = 2; i < 5; ++i ) {
    for ( int j = 0; j < 2; ++j ) {
      Synapses[k++] = [[[GLSynapse alloc] initWithDescription:@"Input -> Hidden Synapse" andInputNeuron:Neurons[j] andOutputNeuron:Neurons[i]] autorelease];
    }
  }

  for ( int i = 2; i < 5; ++i ) {
    Synapses[k++] = [[[GLSynapse alloc] initWithDescription:@"Hidden -> Output Synapse" andInputNeuron:Neurons[i] andOutputNeuron:Neurons[5]] autorelease];
  }


  // Get training data from file
  NSArray *trainingPatterns = [GLPattern patternsWithContentsOfFile:@"XOR.plist"];


  // Train the network until we meet a tolerance constraint for all patterns
  unsigned int iteration       = 0;
  unsigned int trainedPatterns = 0;
  double tolerance             = 0.500;
  double totalError            = 0.000;

  while ( trainedPatterns < 4 ) {
    trainedPatterns = 0;
    totalError      = 0.000;
    iteration++;
    // NSLog( @"iteration => %i", iteration++ ); // DEBUGGING

    for ( int i = 0; i < 4; ++i ) {
      // Set input neuron values
      Neurons[0].value = (double) [[trainingPatterns objectAtIndex:i] inputValues][0];
      Neurons[1].value = (double) [[trainingPatterns objectAtIndex:i] inputValues][1];

      // Forward propagation
      for ( int i = 2; i < 6; ++i ) {
        [Neurons[i] forwardPropagate];
      }

      // Calculate error for output neuron
      [Neurons[5] computeErrorValueWithExpectedValue:[[trainingPatterns objectAtIndex:i] outputValue]];

      // Error calculation & backward propagation
      for ( int i = 4; i > 1; --i ) {
        [Neurons[i] backwardPropagate];
      }

      // Updating weights
      for ( int i = 4; i > 1; --i ) {
        [Neurons[i] updateWeightsForInputNeurons];
      }

      // What did we learn today, class?
      if ( fabs( Neurons[5].errorValue ) < tolerance ) {
        ++trainedPatterns;
      }

      totalError += fabs( Neurons[5].errorValue );
    }
  }


  // Everybody out of the pool!
  [pool drain];
  return 0;
}