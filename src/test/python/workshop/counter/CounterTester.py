import random

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import RisingEdge, Timer
from cocotb.clock import Clock


@cocotb.coroutine
def genClockAndReset(dut):
    dut.reset.value = 1
    yield Timer(1000)
    dut.reset.value = 0
    cocotb.start_soon(Clock(dut.clk, 10, 'ns').start())
    # TODO Animate the dut.clk and dut.reset


@cocotb.test()
def test1(dut):
    cocotb.fork(genClockAndReset(dut))
    counter = 0  # Used to model the hardware
    dut.io_clear.value = 0
    for i in range(256):
        yield RisingEdge(dut.clk)
        # TODO Check that the DUT match with the model (counter variable)
        # read io_value =>     dut.io_value
        # read io_full =>      dut.io_full
        # raise TestFailure("io_value missmatch")
        # raise TestFailure("io_full missmatch")
        if dut.io_value.value != counter:
            raise TestFailure("io_value missmatch")
        if dut.io_full.value != int(counter == 16):
            raise TestFailure("io_full missmatch")
        # TODO Animate the model depending DUT inputs
        if dut.io_clear.value == 1:
            counter = 0
        else:
            counter = (counter + 1) & 0xF
        # TODO Generate random stimulus
        dut.io_clear.value = (random.random() < 0.3)
