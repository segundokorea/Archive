## [Cellular Automata (Ruby Quiz #134)](http://www.rubyquiz.com/quiz134.html)

Simulate elementary cellular automata assigned a Wolfram Code. `cell` runs the simulation and takes upto four arguments: a Wolfram code (default 110), a number of iterations (default 20), a starting configuraiton (default '1'), and a drawing symbol (default |). Use it like so:


    $ ./cell 147 25 11 V
                            VV                        
                           V  V                       
                          V VV V                      
                         V      V                     
                        V VVVVVV V                    
                       V   VVVV   V                   
                      V VVV VV VVV V                  
                     V   V      V   V                 
                    V VVV VVVVVV VVV V                
                   V   V   VVVV   V   V               
                  V VVV VVV VV VVV VVV V              
                 V   V   V      V   V   V             
                V VVV VVV VVVVVV VVV VVV V            
               V   V   V   VVVV   V   V   V           
              V VVV VVV VVV VV VVV VVV VVV V          
             V   V   V   V      V   V   V   V         
            V VVV VVV VVV VVVVVV VVV VVV VVV V        
           V   V   V   V   VVVV   V   V   V   V       
          V VVV VVV VVV VVV VV VVV VVV VVV VVV V      
         V   V   V   V   V      V   V   V   V   V     
        V VVV VVV VVV VVV VVVVVV VVV VVV VVV VVV V    
       V   V   V   V   V   VVVV   V   V   V   V   V   
      V VVV VVV VVV VVV VVV VV VVV VVV VVV VVV VVV V  
     V   V   V   V   V   V      V   V   V   V   V   V 
    V VVV VVV VVV VVV VVV VVVVVV VVV VVV VVV VVV VVV V
