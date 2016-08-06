#!/usr/bin/python2
# -*- coding: utf-8 -*-

from __future__ import print_function, division

import os
import sys
from time import time
import numpy as np
import lasagne
import theano
from theano import tensor as T
from lasagne.layers import InputLayer, DropoutLayer, FlattenLayer, FeaturePoolLayer
from lasagne.layers import DenseLayer, ConcatLayer, SliceLayer, ElemwiseMergeLayer, ElemwiseSumLayer
from lasagne.layers import BatchNormLayer, batch_norm
# from lasagne.layers.dnn import Conv2DDNNLayer as ConvLayer
# from lasagne.layers.dnn import Pool2DDNNLayer as PoolLayer
from lasagne.layers import Pool2DLayer as PoolLayer
from lasagne.layers import Conv2DLayer as ConvLayer 
from lasagne.regularization import regularize_layer_params_weighted, l2, l1
from lasagne.regularization import regularize_layer_params, regularize_network_params 
from lasagne.utils import floatX
from matplotlib import pyplot as plt

def build_raw_model():
    net = {}
    net['input'] = InputLayer((None, 3))
    net['fc1'] = batch_norm(DenseLayer(net['input'], num_units=256))
    net['fc2'] = batch_norm(DenseLayer(net['fc1'], num_units=256))
    net['drop1'] = DropoutLayer(net['fc2'], p=0.5)
    net['fc3'] = batch_norm(DenseLayer(net['drop1'], num_units=256))
    net['fc4'] = batch_norm(DenseLayer(net['fc3'], num_units=256))
    net['drop2'] = DropoutLayer(net['fc4'], p=0.5)
    net['fc_out'] = DenseLayer(net['drop2'], num_units=1)
    return net


def build_pixel_model(input_shape):
    net = {}
    net['input'] = InputLayer((None, 1, input_shape[0], input_shape[1]))
    net['conv1_1'] = ConvLayer(net['input'], num_filters=64, filter_size=3, pad=1, flip_filters=False)
    net['conv1_2'] = ConvLayer(net['conv1_1'], num_filters=64, filter_size=3, pad=1, flip_filters=False)
    net['pool1'] = PoolLayer(net['conv1_2'], pool_size=2, stride=2, mode='max')
    net['conv2_1'] = ConvLayer(net['pool1'], num_filters=128, filter_size=3, pad=1, flip_filters=False)
    net['conv2_2'] = ConvLayer(net['conv2_1'], num_filters=128, filter_size=3, pad=1, flip_filters=False)
    net['pool2'] = PoolLayer(net['conv2_2'], pool_size=2, stride=2, mode='max')
    net['conv3_1'] = ConvLayer(net['pool2'], num_filters=256, filter_size=3, pad=1, flip_filters=False)
    net['conv3_2'] = ConvLayer(net['conv3_1'], num_filters=256, filter_size=3, pad=1, flip_filters=False)
    net['conv3_3'] = ConvLayer(net['conv3_2'], num_filters=256, filter_size=3, pad=1, flip_filters=False)
    net['pool3'] = PoolLayer(net['conv3_3'], pool_size=2, stride=2, mode='max')
    net['conv4_1'] = ConvLayer(net['pool3'], num_filters=512, filter_size=3, pad=1, flip_filters=False)
    net['conv4_2'] = ConvLayer(net['conv4_1'], num_filters=512, filter_size=3, pad=1, flip_filters=False)
    net['conv4_3'] = ConvLayer(net['conv4_2'], num_filters=512, filter_size=3, pad=1, flip_filters=False)
    net['pool4'] = PoolLayer(net['conv4_3'], pool_size=2, stride=2, mode='max')
    net['conv5_1'] = ConvLayer(net['pool4'], num_filters=512, filter_size=3, pad=1, flip_filters=False)
    net['conv5_2'] = ConvLayer(net['conv5_1'], num_filters=512, filter_size=3, pad=1, flip_filters=False)
    net['conv5_3'] = ConvLayer(net['conv5_2'], num_filters=512, filter_size=3, pad=1, flip_filters=False)
    net['pool5'] = PoolLayer(net['conv5_3'], pool_size=2, stride=2, mode='max')
    net['fc6'] = DenseLayer(net['pool5'], num_units=1024)
    net['fc6_dropout'] = DropoutLayer(net['fc6'], p=0.5)
    net['fc7'] = DenseLayer(net['fc6_dropout'], num_units=256)
    net['fc7_dropout'] = DropoutLayer(net['fc7'], p=0.5)
    net['fc_out'] = DenseLayer(net['fc7_dropout'], num_units=1)
    return net


def iterate_minibatches(inputs, targets=None, batch_size=1, shuffle=False):
    if targets is not None:
        assert len(inputs) == len(targets)
    if shuffle:
        indices = np.arange(len(inputs))
        np.random.shuffle(indices)
    for start_idx in range(0, len(inputs), batch_size):
        if shuffle:
            excerpt = indices[start_idx:start_idx + batch_size]
        else:
            excerpt = slice(start_idx, start_idx + batch_size)
        if targets is None:
            yield inputs[excerpt]
        else:
            yield inputs[excerpt], targets[excerpt]


def train_net(net, train_data, train_values, learning_rate=1e-5, max_epoch=1, batch_size=256):
    if len(train_data.shape) == 2:
        data_var = T.fmatrix('data')
    else:
        data_var = T.ftensor4('data')
    values_var = T.fcol('values')
    output_var = lasagne.layers.get_output(net['fc_out'], data_var)
    loss = ((values_var - output_var) ** 2).mean()
    params = lasagne.layers.get_all_params(net['fc_out'], trainable=True)
    updates = lasagne.updates.adam(loss, params,learning_rate=learning_rate)
    train_fn = theano.function([data_var, values_var], loss, updates=updates)
    iteration = 0
    error = []
    for epoch in xrange(max_epoch):
        epoch_err = 0
        for batch in iterate_minibatches(train_data, train_values, batch_size, shuffle=True):
            iteration += 1
            iter_err = train_fn(batch[0], batch[1])
            print('Iteration', iteration, ':', iter_err)
            epoch_err += iter_err
            error = error + [iter_err]
            sys.stdout.flush()
    return error


def get_process_fn(net, data):
    if len(data.shape) == 2:
        data_var = T.fmatrix('data')
    else:
        data_var = T.ftensor4('data')
    output_var = lasagne.layers.get_output(net['fc_out'], data_var, deterministic=True)
    return theano.function([data_var], output_var)


def process(net, data, batch_size=256):
    process_fn = get_process_fn(net, data)
    output = np.zeros((data.shape[0], 1), dtype=np.float32)
    indices = np.arange(len(data))
    for batch_idx in iterate_minibatches(indices, batch_size=batch_size, shuffle=False):
        output[batch_idx] = process_fn(data[batch_idx])
    return output


if __name__ == '__main__':
    np.random.seed(444)
    print('Testing functions')
    image_size = (256, 256)
    raw_net = build_raw_model()
    # pixel_net = build_pixel_model(image_size)
    raw_data = np.random.uniform(size=(1000, 3)).astype(np.float32)
    # pixel_data = np.random.uniform(size=(1000, 1, image_size[0], image_size[1])).astype(np.float32)
    values = np.random.uniform(size=(1000, 1)).astype(np.float32)
    print('Training raw model')
    raw_error = train_net(raw_net, raw_data, values, max_epoch=100)
    # print('Training pixel model')
    # pixel_error = train_net(pixel_net, pixel_data, values, max_epoch=1, batch_size=16)
    print('Processing')
    raw_output = process(raw_net, raw_data)
    print('Error on raw data:', ((raw_output - values) ** 2).mean())
    # pixel_output = process(pixel_net, pixel_data)
    # print('Error on pixel data:', ((pixel_output - values) ** 2).mean())
