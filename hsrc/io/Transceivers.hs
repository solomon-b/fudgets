module Transceivers where
import Socketio
import AsyncTransmitter
import CompOps((>==<),(>=^^<))
import Spops(nullSP)

transmitterF s = closerF s >==< transmitterF' s

receiverF s = receiverF' s >=^^< nullSP

transceiverF s = receiverF' s >==< transmitterF' s

asyncTransceiverF s = receiverF' s >==< asyncTransmitterF' s
