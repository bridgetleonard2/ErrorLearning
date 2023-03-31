#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  3 21:28:17 2022

@author: bridget
"""

import actr

actr.load_act_r_model("ACT-R:tutorial;project;mediator-model.lisp")

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