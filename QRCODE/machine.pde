class Machine {
  PFont font;
  String instructions;
  String buffer;
  int current_configuration;
  ArrayList configurations;
  ArrayList snippets;
  boolean scheduled;
  boolean cycle;
  boolean paused;



  // Initialization
  Machine() {
    font = createFont("VT323", 32);
    instructions = "";
    current_configuration = 0;
    configurations = new ArrayList();
    snippets = new ArrayList();
    scheduled = false;
    cycle = false;
    paused = true;
    textFont(font);
    textAlign(LEFT, BOTTOM);
  }



  // Decrease the value of a tint (or any 0-255 RGB-type value)
  int subTint(int tint, int inc) {
    if (tint - inc < 0) { tint = 0; }
    else { tint -= inc; }
    return tint;
  }

  // Increase the value of a tint (or any 0-255 RGB-type value)
  int addTint(int tint, int inc) {
    if (tint + inc > 255) { tint = 255; }
    else { tint += inc; }
    return tint;
  }



  // Insert a new snippet at the given index (or at the end)
  int addSnippet(int index) {
    if (index >= snippets.size()) {
      snippets.add(instructions);
      return snippets.size() - 1;
    } else if (index >= 0) {
      snippets.set(index, instructions);
    } return -1;
  }

  // Apply a given snippet to the current environment
  void executeSnippet(int index) {
    if ((index >= 0) && (index < snippets.size())) {
      String snippet = (String) snippets.get(index);
      execute(snippet);
      instructions = snippet;
    }
  }



  // Create a new configuration from the current environment
  ArrayList makeConfiguration() {
    return makeConfiguration(rtint, gtint, btint);
  }

  // Create a new configuration from the given environment
  ArrayList makeConfiguration(int rtint, int gtint, int btint) {
    ArrayList config = new ArrayList();
    config.add((float) rtint);
    config.add((float) gtint);
    config.add((float) btint);
    return config;
  }

  // Insert a new configuration at the given index (or at the end)
  int addConfiguration(int index) {
    if (index >= configurations.size()) {
      configurations.add(makeConfiguration());
      return configurations.size() - 1;
    } else if (index >= 0) {
      configurations.set(index, makeConfiguration());
      return index;
    } return -1;
  }

  // Apply a given configuration to the current environment
  int applyConfiguration(int index) {
    if ((index >= 0) && (index < configurations.size())) {
      ArrayList config = (ArrayList) configurations.get(index);
      rtint = int((Float) config.get(0));
      gtint = int((Float) config.get(1));
      btint = int((Float) config.get(2));
      tint(rtint, gtint, btint);
      return index;
    } return -1;
  }



  // Execute the current command line instructions
  void execute() { instructions = execute(instructions); }

  // Execute arbitrary instructions
  String execute(String instructions) {
    String new_instructions = "";
    while (instructions.length() > 0) {
      String instruction = instructions.substring(0,1);
      instructions = instructions.substring(1);

      switch (instruction.charAt(0)) {
        // Clear configurations
        case 'b':
          configurations.clear();
          println("Cleared Configurations.");
          break;

        // Next configuration
        case 'n':
          int i = applyConfiguration(current_configuration + 1);
          if (i >= 0) { current_configuration = i; } else {
            current_configuration = 0;
            applyConfiguration(current_configuration);
          }
          break;

        // Previous configuration
        case 'm':
          int j = applyConfiguration(current_configuration - 1);
          if (j >= 0) { current_configuration = j; } else {
            current_configuration = configurations.size() - 1;
            applyConfiguration(current_configuration);
          }
          break;

        // Start/stop cycling through configurations
        case 'l':
          if (cycle) { cycle = false; } else { cycle = true; }
          break;

        // Start/stop playback
        case 'h':
          if (paused) { audio.play(); } else { audio.pause(); }
          paused = !paused;
          break;

        // Set the scaling factor on the power
        case 'j': scaling /= 1.5; break;
        case 'k': scaling *= 1.5; break;

        // Modify the tint
        case 'w': rtint = addTint(rtint, 25); break;
        case 'q': rtint = subTint(rtint, 25); break;
        case 's': gtint = addTint(gtint, 25); break;
        case 'a': gtint = subTint(gtint, 25); break;
        case 'x': btint = addTint(btint, 25); break;
        case 'z': btint = subTint(btint, 25); break;

        // Apply a configuration
        case '0': applyConfiguration(0); break;
        case '1': applyConfiguration(1); break;
        case '2': applyConfiguration(2); break;
        case '3': applyConfiguration(3); break;
        case '4': applyConfiguration(4); break;
        case '5': applyConfiguration(5); break;
        case '6': applyConfiguration(6); break;
        case '7': applyConfiguration(7); break;
        case '8': applyConfiguration(8); break;
        case '9': applyConfiguration(9); break;


        case 't': useTile = !useTile; break;
        case 'y': otherMode = !otherMode; break;

        // Default: NO-OP
      }
    }

    // Some effects must be applied...
    tint(rtint, gtint, btint);

    // In the future, returns the unexecuted garbage
    return "";
  }



  void keyPressed() {}



  void keyReleased() {
    int index = -1;
    boolean executed = false;

    // Press OPT+[a-z] for single instruction mode
    if (checkKey(KeyEvent.VK_ALT)) {
      for (char letter = 'a'; letter <= 'z'; ++letter) {
        if (checkKey(letter)) {
          execute(charToString(letter));
          executed = true;
        }
      }
    } if (executed) return;

    // Press SHIFT+[0-9] to save the current configuration
    if (checkKey(KeyEvent.VK_SHIFT)) {
      index = 0;
      for (char digit = '0'; digit <= '9'; digit++) {
        if (checkKey(digit)) {
          addConfiguration(index);
          executed = true;
        } ++index;
      }
    } if (executed) return;

    // Press CTRL+[0-9] to apply the given configuration
    if (checkKey(KeyEvent.VK_CONTROL)) {
      index = 0;
      for (char digit = '0'; digit <= '9'; digit++) {
        if (checkKey(digit) && (index < configurations.size())) {
          applyConfiguration(index);
          executed = true;
        } ++index;
      }
    } if (executed) return;

    // Press COMMAND+[0-9] to save the current line to a snippet
    if (checkKey(157 /* COMMAND */)) {
      index = 0;
      for (char digit = '0'; digit <= '9'; digit++) {
        if (checkKey(digit)) {
          addSnippet(index);
          executed = true;
        } ++index;
      }
    } if (executed) return;

    // Press OPT+[0-9] to apply the given snippet
    if (checkKey(KeyEvent.VK_ALT)) {
      index = 0;
      for (char digit = '0'; digit <= '9'; digit++) {
        char data[] = { digit }; String s = new String(data);
        if (checkKey(digit) && (index < snippets.size())) {
          executeSnippet(index);
          executed = true;
        } ++index;
      }
    } if (executed) return;

    // Special copy (CTRL+C), cut (CTRL+X), and paste (CTRL+V) instructions
    if (checkKey(KeyEvent.VK_CONTROL)) {
      if (checkKey(KeyEvent.VK_C)) { executed = true; buffer = instructions; }
      if (checkKey(KeyEvent.VK_X)) { executed = true; buffer = instructions; instructions = ""; }
      if (checkKey(KeyEvent.VK_V)) { executed = true; instructions += buffer; }
    } if (executed) return;

    // Special shortcuts
    if (key == CODED) {
      switch (keyCode) {
        case UP:    executed = true; instructions += 'k'; break;
        case DOWN:  executed = true; instructions += 'j'; break;
        case LEFT:  executed = true; instructions += 'h'; break;
        case RIGHT: executed = true; instructions += 'l'; break;
      }
    } if (executed) return;

    // Command-line interaction
    if (key != CODED) {
      switch (key) {
        // Clear the command line
        case TAB:
          instructions = "";
          break;

        // Remove the last instruction
        case BACKSPACE:
          if (instructions.length() > 0) {
            instructions = instructions.substring(0, instructions.length() - 1);
          }
          break;

        // Execute the line (ASAP)
        case ';':
        case ENTER:
        case RETURN:
          scheduled = true;
          break;

        // Finally, nothing fishy going on, just add to our instruction stack
        default: instructions += key;
      }
    }
  }



  void draw() {
    pushMatrix();

      // Draw the command line
      text(instructions, 0,height);

      // Execute our stack if called for
      if (scheduled) { execute(); scheduled = false; }

      // Cycle through configuraitons if called for
      if (cycle) {
        if (current_configuration < configurations.size()) {
          applyConfiguration(current_configuration++);
        } else { current_configuration = 0; }
      }

    popMatrix();
  }



  void stop() {}
}
