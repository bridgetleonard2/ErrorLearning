import numpy as np
import scipy.optimize as opt
import pandas as pd
import math
import scipy as sp


def activation(traces, time, decay):
    """Computes the activation of a memory given its history of retrievals"""
    ftraces = [x for x in traces if x < time]
    decay = max(0, decay)  # Allows no positive decay rates in equation
    decay - min(decay, 5)
    times = time - np.array(ftraces)
    odds = times ** -decay
    return np.log(np.sum(odds))


def boltzmann(options, values, temperature):
    """Returns a Boltzmann distribution of the probabilities of each option"""
    temperature = max(temperature, 0.01)
    vals = np.array(values)/temperature
    # bvals = np.exp(vals)/np.sum(np.exp(vals))
    bvals = np.exp(vals - np.max(vals)) / np.exp(vals - np.max(vals)).sum()
    return dict(zip(options, bvals))


def responsetime(activation, ter, F=1, f=1):
    return ter + F * np.exp(-f * activation)


def rtProb(rt, activation, s, ter):
    """Takes one parameter for noise, s, and outputs a probability
    distribution for response times"""
    noise = np.linspace(-2, 2)
    dist = sp.stats.logistic(0, ((math.pi**2)*s)/3)
    rts = [responsetime((activation - x), ter) for x in noise]
    prob = dist.pdf(noise)
    rtprob = {rts[i]: prob[i]for i in range(len(noise))}
    val = min(rtprob.keys(), key=lambda x: abs(x - (rt/1000)))
    return rtprob[val]


def LLelabRT(alldata, ppt, decay, temp, ter, mas=1.6):
    data = alldata[alldata.participant == ppt]
    # create a list of error items
    errors = data[data.condition == 1].cue.tolist()
    # create a list of study items
    study = data[data.condition == 2].cue.tolist()
    pos = 1
    present = errors[:]
    for i in range(len(errors)):
        word = study[i]
        present.insert(pos, word)
        pos += 2
    # Create dict with word pairs
    pairs = {}
    for cue, target in zip(data.cue, data.target):
        pairs[cue] = target
    # also create a dict with errors
    errorResp = dict()
    for cue, response in zip(data.cue, data.study_response):
        if isinstance(response, str):
            errorResp[cue] = response

    # model learning phase, encode a single trace for each item:
    DM = dict()
    # for DM can we make a dictionary of dictionaries where big keys are cues,
    # values are dictionary of target/possible responses and their activation
    time = 0
    for cue in present:
        littleDM = {}
        study_responses = alldata[alldata.cue == cue]['study_response']
        test_responses = alldata[alldata.cue == cue]['test_response']
        # make a set of all reponses given to a certain cue to be
        # "vocab for that cue"
        for response in set(pd.concat([study_responses, test_responses])):
            if isinstance(response, str):
                littleDM[response] = [0.001]
            # add retrieval of error for error items
            if cue in errorResp.keys():
                error = errorResp[cue]
                time += 5
                littleDM[error] = [0.001, time]
                # overwrite smaller activ of correct target to show
                # task learning
                time += 5
                littleDM[pairs[cue]] = [0.001, time]
            else:
                time += 10
                littleDM[pairs[cue]] = [0.001, time]
            DM[cue] = littleDM
    time += 300  # time for distractor phase

    # model testing phase
    LL = 0
    for condition, cue, target, \
        response, rt, feedback in zip(data.condition,
                                      data.cue,
                                      data.target,
                                      data.test_response,
                                      data.test_rt,
                                      data.correct):
        # Calculate log likelihood of response- possible options are 19 random
        # integers or correct associate
        options = DM[cue].keys()
        # create spreading activation additional error component given size of
        # cue's dec mem
        cueMem = len(DM[cue])
        add = (mas - np.log((cueMem + 1)/2)) - (mas - np.log((cueMem + 1)/1))
        # if error condition, add spreading activation
        values = [(activation(DM[cue][opt], time, decay) + add) if
                  condition == 1 else activation(DM[cue][opt], time, decay)
                  for opt in options]

        # Set default value to be a random item in the dict:
        prob = boltzmann(options, values, temp)[response]

        # now calculate response times:
        if condition == 1:
            resp_activation = activation(DM[cue][response], time, decay) + add
        else:
            resp_activation = activation(DM[cue][response], time, decay)

        prob_rt = rtProb(rt, resp_activation, temp, ter)

        # Sum up the LLs
        LL += (np.log(max(prob, 10e-10)) + np.log(max(prob_rt, 10e-10)))

        # add time taken to responde
        time += rt/1000
    return LL


def rtProb2(rt, resp_activation, error_activation, condition, s, ter):
    """Takes one parameter for noise, s, and outputs a probability
    distribution for response times"""
    noise = np.linspace(-2, 2)
    dist = sp.stats.logistic(0, ((math.pi**2)*s)/3)
    if condition == 1:
        rts = [(responsetime((resp_activation - x), ter) +
                responsetime((error_activation - x), ter)) for x in noise]
    else:
        rts = [responsetime((resp_activation - x), ter) for x in noise]
    prob = dist.pdf(noise)
    rtprob = {rts[i]: prob[i]for i in range(len(noise))}
    val = min(rtprob.keys(), key=lambda x: abs(x - (rt/1000)))
    return rtprob[val]


def LLmedRT(alldata, ppt, decay, temp, ter):
    """For each trial, calculate the probability of that response,
    sum the log likelihoods, and update the values"""
    data = alldata[alldata.participant == ppt]
    # create a list of error items
    errors = data[data.condition == 1].cue.tolist()
    # create a list of study items
    study = data[data.condition == 2].cue.tolist()
    pos = 1
    present = errors[:]
    for i in range(len(errors)):
        word = study[i]
        present.insert(pos, word)
        pos += 2

    # Create dict with word pairs
    pairs = {}
    for cue, target in zip(data.cue, data.target):
        pairs[cue] = target
    # also create a dict with errors
    errorResp = dict()
    for cue, response in zip(data.cue, data.study_response):
        if isinstance(response, str):
            errorResp[cue] = response

    # model learning phase, encode a single trace for each item
    DM = dict()
    # for DM can we make a dictionary of dictionaries where big keys are cues,
    # values are dictionary of target/possible responses and their activation
    time = 0
    for cue in present:
        littleDM = {}
        study_responses = alldata[alldata.cue == cue]['study_response']
        test_responses = alldata[alldata.cue == cue]['test_response']
        # make a set of all reponses given to a certain cue to be
        # "vocab for that cue"
        for response in set(pd.concat([study_responses, test_responses])):
            if isinstance(response, str):
                littleDM[response] = [0.001]
        # add retrieval of error for error items
        if cue in errorResp.keys():
            error = errorResp[cue]
            time += 5
            littleDM[error] = [0.001, time]
            # overwrite smaller activ of correct target to show task learning
            time += 5
            littleDM[pairs[cue]] = [0.001, time]
        else:
            time += 10
            littleDM[pairs[cue]] = [0.001, time]
        DM[cue] = littleDM
    time += 300  # time for distractor phase

    # model testing phase
    LL = 0

    for condition, cue, target, \
        response, rt, feedback in zip(data.condition,
                                      data.cue,
                                      data.target,
                                      data.test_response,
                                      data.test_rt,
                                      data.correct):
        # Calculate log likelihood of response-
        # possible options are 19 random integers
        # or correct associate
        options = DM[cue].keys()

        # calculate probability of retrieving given response
        values = [activation(DM[cue][opt], time, decay) for opt in options]
        prob1 = boltzmann(options, values, temp)[response]

        # probability of retrieving error memory
        if condition == 1:
            error = errorResp[cue]
            prob2 = boltzmann(options, values, temp)[error]
        else:
            prob2 = 0

        # add response times calculations
        # probability of given response time with
        respAct = activation(DM[cue][response], time, decay)
        if condition == 1:
            error = errorResp[cue]
            errorAct = activation(DM[cue][error], time, decay)
            prob_rt = rtProb2(rt, respAct, errorAct, condition, temp, ter)
        else:
            errorAct = 0
            prob_rt = rtProb2(rt, respAct, errorAct, condition, temp, ter)

        # Sum up the LLs
        LL += (np.log(max(prob1 + prob2, 10e-10)) +
               np.log(max(prob_rt, 10e-10)))

        # add time taken to responde
        time += rt/1000

    return LL


def vLLelab(array, data, ppt):
    """Vector function of procedural log-likelihood"""
    decay, temp, ter = array
    return -1 * LLelabRT(data, ppt, decay, temp, ter)


def vLLmed(array, data, ppt):
    """Vector function of procedural log-likelihood"""
    decay, temp, ter = array
    return -1 * LLmedRT(data, ppt, decay, temp, ter)


def ll_participant(alldata, ppt, LL_data):
    data = alldata[alldata.participant == ppt]
    print(data)
    edecay, etemp, eter = opt.minimize(vLLelab, x0=[0.5, 1, 1],
                                       args=(data, ppt),
                                       method="Powell",
                                       bounds=[[0.01, 2], [0, 2], [0.1, 2]]).x
    llelab = LLelabRT(alldata, ppt, edecay, etemp, eter)

    mdecay, mtemp, mter = opt.minimize(vLLmed, x0=[0.5, 1, 1],
                                       args=(data, ppt),
                                       method="Powell",
                                       bounds=[[0.01, 2], [0, 2], [0.1, 2]]).x
    llmed = LLmedRT(alldata, ppt, mdecay, mtemp, mter)

    best = "Mediator"
    if llelab > llmed:
        best = "Elaborative"

    diff = llmed - llelab

    row = [ppt, edecay, etemp, eter, llelab,
           mdecay, mtemp, mter, llmed, best, diff]

    LL_data = pd.concat([LL_data, pd.DataFrame([row],
                                               columns=LL_data.columns)],
                        ignore_index=True)

    return row, LL_data
