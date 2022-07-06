import random
from queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import RisingEdge, Timer


@cocotb.coroutine
def genClockAndReset(dut):
    dut.reset.value = 1
    dut.clk.value = 0
    yield Timer(1000)
    dut.reset.value = 0
    yield Timer(1000)
    while True:
        dut.clk.value = 1
        yield Timer(500)
        dut.clk.value = 0
        yield Timer(500)


@cocotb.coroutine
def driverAgent(dut):
    dut.io_push_valid.value = 0
    dut.io_pop_ready.value = 0
    dut.io_push_payload.value = 0
    while True:
        yield RisingEdge(dut.clk)
        # TODO generate random stimulus on the hardware
        dut.io_push_valid.value = random.random() < 0.3
        dut.io_pop_ready.value = random.random() < 0.3
        dut.io_push_payload.value = random.randint(0, (1 << 8) - 1)


@cocotb.coroutine
def checkerAgent(dut):
    queue = Queue()
    matchCounter = 0
    while matchCounter < 5000:
        yield RisingEdge(dut.clk)
        # TODO Capture and store 'push' transactions into the queue
        if dut.io_push_valid.value and dut.io_push_ready.value:
            queue.put(int(dut.io_push_payload.value))  # !!!
        # TODO capture and check 'pop' transactions with the head of the queue.
        # If match increment matchCounter else throw error
        if dut.io_pop_valid.value and dut.io_pop_ready.value:
            if queue.empty():
                raise TestFailure("parasite io_pop transaction")
            elif dut.io_pop_payload.value != queue.get():
                raise TestFailure(f"io_pop_payload mismatch")
            else:
                matchCounter += 1


@cocotb.test()
def test1(dut):
    # Create all threads
    cocotb.fork(genClockAndReset(dut))
    cocotb.fork(driverAgent(dut))
    checker = cocotb.fork(checkerAgent(dut))
    # Wait until the checker finish his job
    yield checker.join()
