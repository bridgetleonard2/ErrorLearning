import actr

actr.load_act_r_model("ACT-R:project;elaborative-model.lisp")

# Create variables to hold model's response times, the time of that response,
# the possible stimuli, and the data from the behavioral task

response = False
response_time = False

pairs = list(zip(['adventure', 'bagel', 'candle', 'direction', 'home',
                  'octopus', 'potato', 'stack', 'wreck'],
                 ['trip', 'butter', 'wick', 'arrow', 'sick', 'ocean',
                  'mash', 'build', 'ship']))

# behavioral_data_here =


# respond_to_key_press is set to monitor the output-key command
# and records the time and key that was pressed by the model.

def respond_to_space_press (model,key):
    global response_time
    
    response_time = actr.get_time()

def respond_to_key_press (model,key):
    global response

    response = key

# sentence function runs a simple singular trial in which a cue, target, and result (whether cue and target pair
# if T or F) is entered

def sentence(cue, target):

    actr.reset()

    window = actr.open_exp_window("Retrieval Experiment", visible=False)

    actr.install_device(window)

    actr.add_command("cue-response", respond_to_space_press, "Retrieval experiment model response to cue")
    actr.monitor_command("output-key","cue-response")

    actr.add_text_to_exp_window(window, cue)

    global response_time

    response_time = 0

    # cue is presented -- retrieval of target begins, once some chunnk retrieved -- press key ?

    actr.run(30)
    
    actr.remove_command_monitor("output-key", "cue-response")
    actr.remove_command("cue-response")

    actr.clear_exp_window(window)
    
    actr.add_command("feedback-response", respond_to_key_press, "Retrieval experiment model response to feedback")
    actr.monitor_command("output-key","feedback-response")

    actr.add_text_to_exp_window(window, target)

    global response

    response = ''

    actr.run(30)
    
    actr.remove_command_monitor("output-key", "feedback-response")
    actr.remove_command("feedback-response")
    
    if response == 'f':
        return (True, response_time / 1000)
    elif response == 'j':
        return (False, response_time / 1000)

    # feedback provided in form of correct target -- if match response: press f, if no match press j
    
# make a function that runs an experiment given two parameters.
# the first indicates the number of pairs to use. the second
# indicates the number of trials to run.

def do_experiment(size,trials):
    
    actr.reset()
    
    # create a variable to hold the data
    
    result = []
    
    # install the virtual window for the model
    
    window = actr.open_exp_window("Retrieval Experiment", visible=False)

    actr.install_device(window)
    
    # loop over the number of trials
    # score will increase 1 if correct, 0 if wrong
    # time will gather response times (maybe split into correct vs 
    # incorrect later)
    
    for i in range(trials):
        score = 0
        time = 0
        
        for cue,target in actr.permute_list(pairs[9 - size:]):
            
            
            # clear window and display the prompt -- run bulk of 
            # sentence function here
            
            actr.clear_exp_window(window)
            
            actr.add_command("cue-response", respond_to_space_press, "Retrieval experiment model response to cue")
            actr.monitor_command("output-key","cue-response")
            
            actr.add_text_to_exp_window(window, cue)
            
            global response_time

            response_time = 0
            start = actr.get_time()

            # cue is presented -- retrieval of target begins, once some chunnk retrieved -- press key ?

            actr.run(30)
            
            actr.remove_command_monitor("output-key", "cue-response")
            actr.remove_command("cue-response")

            actr.clear_exp_window(window)
            
            actr.add_command("feedback-response", respond_to_key_press, "Retrieval experiment model response to feedback")
            actr.monitor_command("output-key","feedback-response")

            actr.add_text_to_exp_window(window, target)

            global response

            response = ''

            actr.run(30)
            
            actr.remove_command_monitor("output-key", "feedback-response")
            actr.remove_command("feedback-response")
            
            # If there is a correct response increment the
            # count of correct answers and the cumulative
            # response times.
            
            if response == 'f':
                score += 1
                time += response_time - start
                
            if response == 'j':
                time += response_time - start
        
        # Record the score and time data in the result list
        
        result.append((score/size, time/size/1000))
        
    return result
            
    
    
   
def do_cue(trials):
     
     # create a variable to hold the data
     
     result = []
     
     # install the virtual window for the model
     
     window = actr.open_exp_window("Retrieval Experiment", visible=False)

     actr.install_device(window)
     
     # loop over the number of trials
     # score will increase 1 if correct, 0 if wrong
     # time will gather response times (maybe split into correct vs 
     # incorrect later)
     
     for i in range(trials):
         score = 0
         time = 0
         
         for cue,target in actr.permute_list(pairs):
             
             
             # clear window and display the prompt -- run bulk of 
             # sentence function here
             
             actr.clear_exp_window(window)
             
             actr.add_command("cue-response", respond_to_space_press, "Retrieval experiment model response to cue")
             actr.monitor_command("output-key","cue-response")
             
             actr.add_text_to_exp_window(window, cue)
             
             global response_time

             response_time = 0
             start = actr.get_time()

             # cue is presented -- retrieval of target begins, once some chunnk retrieved -- press key ?

             actr.run(30)
             
             actr.remove_command_monitor("output-key", "cue-response")
             actr.remove_command("cue-response")

             actr.clear_exp_window(window)
             
             actr.add_command("feedback-response", respond_to_key_press, "Retrieval experiment model response to feedback")
             actr.monitor_command("output-key","feedback-response")

             actr.add_text_to_exp_window(window, target)

             global response

             response = ''

             actr.run(30)
             
             actr.remove_command_monitor("output-key", "feedback-response")
             actr.remove_command("feedback-response")
             
             # If there is a correct response increment the
             # count of correct answers and the cumulative
             # response times.
             
             if response == 'f':
                 score += 1
                 time += response_time - start
                 
             if response == 'j':
                 time += response_time - start
         
         # Record the score and time data in the result list
         
         result.append((score/9, time/9/1000))
         
     return result   
   
    
    # make an experiment function that runs the model through the paradigm
    
    # i want the output to be a table: x = wordpair y = phase values = performance (correct/incorr & RT)



def do_sentence(cue,target):

    window = actr.open_exp_window("Retrieval Experiment", visible=False)

    actr.install_device(window)

    actr.add_command("cue-response", respond_to_space_press, "Retrieval experiment model response to cue")
    actr.monitor_command("output-key","cue-response")

    actr.add_text_to_exp_window(window, cue)

    global response_time

    response_time = 0
    start = actr.get_time()

    # cue is presented -- retrieval of target begins, once some chunnk retrieved -- press key ?

    actr.run(30)
    
    actr.remove_command_monitor("output-key", "cue-response")
    actr.remove_command("cue-response")

    actr.clear_exp_window(window)
    
    actr.add_command("feedback-response", respond_to_key_press, "Retrieval experiment model response to feedback")
    actr.monitor_command("output-key","feedback-response")

    actr.add_text_to_exp_window(window, target)

    global response

    response = ''

    actr.run(30)
    
    actr.remove_command_monitor("output-key", "feedback-response")
    actr.remove_command("feedback-response")
    
    rt = response_time - start
    
    if response == 'f':
        return (cue, True, rt / 1000)
    elif response == 'j':
        return (cue, False, rt / 1000)
    
    

# experiment runs the model through one trial of 
# each condition using each of the retrieval productions
# and averages the results then displays the results.



def true_experiment():
     
     result = []
            
     for cue,target in [('adventure', 'trip'),
                        ('bagel', 'butter'), 
                        ('candle', 'wick'), 
                        ('direction', 'arrow'),
                        ('home','sick'), 
                        ('octopus', 'ocean'),  
                        ('potato', 'mash'),
                        ('stack', 'build'),
                        ('wreck', 'ship')]:
             
             result.append(do_sentence(cue,target))
         
     return result   
   


   
    
   
    
   
    
   
    
   
    
   
    

    
