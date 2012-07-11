import com.aos.zxing4processing.*; // For QR code generation
import ddf.minim.analysis.*;       // For spectrum analysis
import ddf.minim.*;                // For audio input/output


// Global objects
Machine mach;
ZXING4P zxing;
PImage qrcode;
PImage tile;
Minim minim;
AudioPlayer audio;
AudioPlayer shitty_audio;
FFT fft;
FFT shitty_fft;
int divs;
int ldivs;
int leftMargin;
int len;

// Essentially constants
int frameRate = 10;
int sampleRate = 8;
String audio_file = "blast.mp3";
String tileName = "shabazz.jpg";
boolean useTile = true;
boolean otherMode = false;

// Instantaneous configuration
float scaling = 0.75;
int rtint     =  255;
int gtint     =  255;
int btint     =  255;


// Useful for handling multiple keypresses
boolean[] keys = new boolean[526];

String charToString(char c) {
  char data[] = { c }; return new String(data);
}

boolean checkKey(char k) {
  for (int i = 0; i < keys.length; ++i) {
    if (KeyEvent.getKeyText(i).toLowerCase().equals(charToString(k).toLowerCase())) {
      return keys[i];
    }
  } return false;
}

boolean checkKey(int k) {
  if (k < keys.length) {
    return keys[k];
  } return false;
}


// Write a FFT spectrum to a string
String getFFTString(FFT fft) {
  int size = fft.specSize();
  String result = "";
  float amplitude;
  for (int i = 0; i < size; ++i) {
    amplitude = audio.mix.get(i);
    result   += nf(amplitude * 100, 4, 2);
  } return result;
}



void setup() {
  // Try to divide the screen into a grid for the images
  divs = 1;
  len = screen.height;
  ldivs = floor(screen.width / len);
  size(screen.width, len, P3D);
  frameRate(frameRate);
  noStroke();
  // smooth();

  // Initialize the big global objects
  zxing        = new ZXING4P(this);
  minim        = new Minim(this);
  audio        = minim.loadFile(audio_file, 2048);
  shitty_audio = minim.loadFile(audio_file, sampleRate);
  fft          = new FFT(audio.bufferSize(), audio.sampleRate());
  shitty_fft   = new FFT(shitty_audio.bufferSize(), shitty_audio.sampleRate());
  mach         = new Machine();

  // "audio" is used for everything except the QR code
  // "shitty_audio" actually fits inside the code, but it's out of sync with "audio"
  // Too bad...
  audio.pause();
  shitty_audio.loop();
  shitty_audio.mute();

  fill(rtint, gtint, btint);
  tint(rtint, gtint, btint);
  tile = loadImage(tileName);
}


void draw() {
  // Run the spectrum analysis
  fft.forward(audio.mix);
  shitty_fft.forward(shitty_audio.mix);

  // Calculate the instantaneous "power" of the beat
  float power = 0;
  for (int i = 0; i < fft.specSize() / 3; ++i) {
    power += fft.getBand(i) * scaling;
  }
  divs = floor(power / 300) + 1;

  // Recalculate environment
  fill(rtint, gtint, btint);
  rect(0,0, width,height);
  len = height / divs;
  ldivs = floor(width / len);
  leftMargin = (width - len * ldivs) / 2;

  if (!otherMode) {
    // Write out image matrix
    pushMatrix();
      translate(leftMargin,0);
      if (!useTile) { qrcode = zxing.generateQRCode(getFFTString(shitty_fft), len,len); }
      for (int i = 0; i < ldivs; ++i) {
        for (int j = 0; j < divs; ++j) {
          if (useTile) { image(tile, 0,0, len,len); }
          else { image(qrcode, 0,0, len,len); }
          translate(0, len);
        }
        translate(len, -divs * len);
      }
    popMatrix();
  }

  filter(INVERT); // Looks a little nicer
  mach.draw();

  if (otherMode) {
    // Draw waveform background
    pushMatrix();
      translate(0, height / 2);
      noFill();
      stroke(rtint, gtint, btint);
      strokeWeight(3);
      for(int i = 0; i < audio.bufferSize() - 1; ++i) {
        line(i, audio.mix.get(i)*100, i+1, audio.mix.get(i+1)*100);
      }
    popMatrix();
  
    // Draw the "sun"
    pushMatrix();
      translate(width / 2, height / 2);
      noStroke();
      fill(rtint, gtint, btint);
      ellipseMode(RADIUS);
      ellipse(0, 0, 10 * divs, 10 * divs);
      rectMode(CORNER);
      for(int j = 0; j < fft.specSize(); ++j) {
        float fraction = float(j) / fft.specSize();
        rotateZ(fraction * TWO_PI);
        rect(0, 10 * divs, 2, fft.getBand(j) * 3.0);
      }
    popMatrix();
  }
}


void keyPressed() {
  keys[keyCode] = true;
  mach.keyPressed();
}


void keyReleased() {
  mach.keyReleased();
  keys[keyCode] = false;
}


void stop() {
  audio.close();
  shitty_audio.close();
  minim.stop();
  mach.stop();
  super.stop();
}
