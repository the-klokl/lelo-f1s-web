
var app = Elm.BtLelo.init({
    node: document.getElementById('myapp')
});

var device = null
var gatt;

var intervals = []

let leloServiceId = 0xFFF0
let batteryServiceId = 0x180F
let deviceInformationServiceId = 0x180A

async function initConnection() {

    device = await navigator.bluetooth.requestDevice({
        filters: [{
            services: [leloServiceId]
        }],
        optionalServices: [
            leloServiceId, batteryServiceId, deviceInformationServiceId
        ]
    })

    gatt = await device.gatt.connect()

    device.addEventListener('gattserverdisconnected', e => {
        console.log("disconnected: " + e)
        intervals.map(clearInterval)

        app.ports.disconnected.send(0);
    })

    app.ports.foundDevice.send(device.name)

    let leloService = await gatt.getPrimaryService(0xFFF0)

    let securityAccessChar = await leloService.getCharacteristic(0x0A10)

    let securityAccessValue = await securityAccessChar.readValue()

    if (securityAccessValue.getBigUint64() == 00) {
        app.ports.securityAcceptRequired.send(1)
    }

    while (securityAccessValue.getBigUint64() == 00) {
        await sleep(1000)
        securityAccessValue = await securityAccessChar.readValue()
    }

    await securityAccessChar.writeValue(securityAccessValue);

    await sleep(1000)

    securityAccessValue = await securityAccessChar.readValue();

    if (securityAccessValue.getInt8(0) != 01) {
        app.ports.errorConnecting.send("could not authorize")
    } else {
        app.ports.fullyInitiated.send(device.name)

        await setupServiceNotifications()
        readAndSendMotorSpeed()
        portSubscriptions()
    }

}

async function portSubscriptions() {

    let motorControlCh = await gatt.getPrimaryService(leloServiceId).then(s => s.getCharacteristic(0xFFF1))

    let shutdown = app.ports.shutdown.subscribe(() => {
        let dv = new DataView(new ArrayBuffer(2))
        dv.setUint16(0, 0x01FA)

        motorControlCh.writeValue(dv)
    })

    let stopMotors = app.ports.stopMotors.subscribe(() => {
        let dv = new DataView(new ArrayBuffer(2))
        dv.setUint16(0, 0x01FF)

        motorControlCh.writeValue(dv)
    })

    let calibrateAccelerometer = app.ports.calibrateAccelerometer.subscribe(() => {
        let dv = new DataView(new ArrayBuffer(3))
        dv.setUint16(0, 0xFFFF)
        dv.setUint8(2, 0xFF)

        motorControlCh.writeValue(dv)
    })

    let setMotorSpeed = app.ports.setMotorSpeed.subscribe(motorSpeed => {
        console.log(motorSpeed)

        let mainMotorPerc = Math.max(motorSpeed.mainMotor, 0)
        let vibrationMotorPerc = Math.max(motorSpeed.vibrationMotor, 0)

        let mainMotor = Math.min(Math.ceil(0.64 * mainMotorPerc), 64)
        let vibrationMotor = Math.min(Math.ceil(0.64 * vibrationMotorPerc), 64)

        let dv = new DataView(new ArrayBuffer(3))

        dv.setUint8(0, 01)
        dv.setUint8(1, mainMotor)
        dv.setUint8(2, vibrationMotor)

        motorControlCh.writeValue(dv)
    })

    device.addEventListener('gattserverdisconnected', e => {
        console.log("disconnected: " + e)

        app.ports.shutdown.unsubscribe(shutdown);
        app.ports.stopMotors.unsubscribe(stopMotors);
        app.ports.calibrateAccelerometer.unsubscribe(calibrateAccelerometer);
        app.ports.setMotorSpeed.unsubscribe(setMotorSpeed);
    })

}

async function readAndSendMotorSpeed() {
    let leloService = await gatt.getPrimaryService(leloServiceId)
    let motorControlCH = await leloService.getCharacteristic(0xFFF1)

    async function repeated() {
        if (gatt && gatt.connected) {
            let value = await motorControlCH.readValue()

            let mainMotorSpeed = value.getUint8(1)
            let vibratorMotorSpeed = value.getUint8(2)

            app.ports.updateMotorSpeed.send({ mainMotor: mainMotorSpeed, vibrationMotor: vibratorMotorSpeed })
        }
    }


    let interval = setInterval(repeated, 1000)
    intervals = [...intervals, interval]

}

async function readCharacteristicOnInterval(serviceId, characteristicId, interval, characteristicName, characteristicTransformer) {
    gatt.getPrimaryService(serviceId).then(service =>
        readCharacteristicOnIntervalWithService(service, characteristicId, interval, characteristicName, characteristicTransformer)
    )
}

async function readCharacteristicOnIntervalWithService(service, characteristicId, interval, characteristicName, characteristicTransformer) {
    if (gatt && gatt.connected) {
        let characteristicValue = await service.getCharacteristic(characteristicId).then(x => x.readValue())

        let v = characteristicTransformer(characteristicValue)

        sendIntCharacteristicUpdate(characteristicName, v)

        setTimeout(readCharacteristicOnIntervalWithService(service, characteristicId, interval, characteristicName, characteristicTransformer), interval)
    }
}

function sendIntCharacteristicUpdate(charName, charValue) {

    app.ports.characteristicIntUpdate.send([charName, charValue])

}

async function setupServiceNotifications() {

    subscribeToCharacteristic(batteryServiceId, 0x2A19, 'batteryLevel', x => x.getInt8(0))

    let leloService = await gatt.getPrimaryService(leloServiceId)

    subscribeToCharacteristicWithService(leloService, 0x0A0B, "depth", x => x.getInt16(0))
    subscribeToCharacteristicWithService(leloService, 0x0AA3, "hall", x => x.getInt16(0))

    await setupPositionUpdates(leloService);

}


// accelerometer
async function setupPositionUpdates(leloService) {
    let characteristic = await leloService.getCharacteristic(0x0A0C).then(s => s.startNotifications())

    characteristic.addEventListener('characteristicvaluechanged',
        event => {
            let buff = event.target.value
            let x = buff.getUint16(0)
            let y = buff.getUint16(2)
            let z = buff.getUint16(4)

            let ww = buff.getUint8(6)

            let triggerFlag = 0b10000000 & ww

            let orient2 = 0b01000000 & ww
            let orient1 = 0b00100000 & ww
            let orient0 = 0b00010000 & ww

            let xDirection = 0b00000100 & ww
            let yDirection = 0b00000010 & ww
            let zDirection = 0b00000001 & ww

            app.ports.updatePosition.send({
                x: xDirection ? x : -x,
                y: yDirection ? y : -y,
                z: zDirection ? z : -z,
                direction: orient2 ? true : false
            })
        })
}

async function subscribeToCharacteristic(serviceId, characteristicId, characteristicName, characteristicTransformer) {
    let service = await gatt.getPrimaryService(serviceId)

    subscribeToCharacteristicWithService(service, characteristicId, characteristicName, characteristicTransformer)
}

async function subscribeToCharacteristicWithService(service, characteristicId, characteristicName, characteristicTransformer) {
    subscribeToCharacteristicAndCustomInterpret(service, characteristicId, buff => [[characteristicName, characteristicTransformer(buff)]])
}

/** @argument characteristicTransformer (arrayBuffer -> [(nameOfChar, valueOfChar)]) */
async function subscribeToCharacteristicAndCustomInterpret(service, characteristicId, characteristicTransformer) {
    let characteristic = await service.getCharacteristic(characteristicId).then(s => s.startNotifications())

    characteristic.addEventListener('characteristicvaluechanged',
        event => {
            characteristicTransformer(event.target.value).map(tup =>
                sendIntCharacteristicUpdate(tup[0], tup[1])
            )
        })
}

app.ports.initConnection.subscribe(function () {
    initConnection().catch(err => {
        app.ports.errorConnecting.send(err.toString())

        if (gatt) {
            gatt.disconnect()
        } else if (device) {
            device.gatt.disconnect()
        }

        console.error(err)
    })
})

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms))
}

