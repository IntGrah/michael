import { Elm } from './Main.elm';
import ElmVexFlow from './elm-vexflow';

// ---- Custom element ----

customElements.define('elm-vexflow', ElmVexFlow);

// ---- Elm initialisation ----

const app = Elm.Main.init({
    node: document.getElementById('app')!,
});

// ---- WebSockets ----

const socket = new WebSocket(`ws://${import.meta.env.VITE_WS_SERVER}/ws`);

app.ports.internalSend.subscribe((message) => {
    if (socket.readyState === WebSocket.OPEN) {
        console.log(JSON.stringify(message));
        socket.send(JSON.stringify(message));
    } else {
        console.error('Tried to send to a closed socket');
    }
});

socket.onmessage = (event) => {
    console.log(event.data);
    const data = JSON.parse(event.data);
    app.ports.internalReceive.send(data);
};

socket.onclose = () => {
    app.ports.internalReceive.send(['Error', 'Socket closed!']);
};

// ---- Metronome beeps ----

const ctx = new AudioContext();

const gainNode = ctx.createGain();
gainNode.connect(ctx.destination);
gainNode.gain.setValueAtTime(0, ctx.currentTime);

const oscillator = ctx.createOscillator();
oscillator.type = 'sine';
oscillator.connect(gainNode);
oscillator.start();
const smoothing = 0.02;

app.ports.beep.subscribe((freq: number) => {
    oscillator.frequency.value = freq;
    gainNode.gain.setTargetAtTime(1, ctx.currentTime, smoothing);
    gainNode.gain.setTargetAtTime(0, ctx.currentTime + 0.05, smoothing);
});
