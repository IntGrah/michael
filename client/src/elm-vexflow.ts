import {
    Articulation,
    Beam,
    Formatter,
    Fraction,
    Renderer,
    Stave,
    StaveNote,
    Tuplet,
} from 'vexflow';

/**
 * Will be called by Elm.
 *
 * Register a custom DOM element that renders VexFlow content, passed as a string attribute in JSON format.
 */
export default class ElmVexFlow extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        this.render();
    }

    attributeChangedCallback() {
        this.render();
    }

    static get observedAttributes() {
        return ['data'];
    }

    render() {
        // Type assumption
        type Note =
            | { type: 'note'; pitch: string; duration: string; accent: boolean }
            | { type: 'rest'; duration: string };
        type Beat = { data: Note[]; triplet: boolean };
        type Data = { beat: Beat; changed: boolean }[];

        const data = this.getAttribute('data') ?? '[]';
        const j = JSON.parse(data) as Data;

        if (this.lastChild !== null) this.removeChild(this.lastChild);
        const div = document.createElement('div');
        div.style.width = '540px';
        div.style.height = '240px';

        this.appendChild(div);
        const renderer = new Renderer(div, Renderer.Backends.SVG);
        renderer.resize(540, 240);
        const context = renderer.getContext();

        const stave = new Stave(20, 60, 500);
        stave.setContext(context).draw();

        const allNotes: StaveNote[] = [];
        const allBeams: Beam[] = [];
        const allTuplets: Tuplet[] = [];

        for (const { beat, changed } of j) {
            const colour = changed ? '#c43535' : '#000000';

            // ---- Notes ----
            const notes = beat.data.map((x) => {
                let note: StaveNote;
                if (x.type === 'note') {
                    note = new StaveNote({
                        keys: [x.pitch],
                        duration: x.duration,
                    });
                    if (x.accent) note.addModifier(new Articulation('\uE4A1'));
                } else {
                    note = new StaveNote({
                        keys: ['b/4'],
                        duration: x.duration + 'r',
                    });
                }

                note.setStyle({ fillStyle: colour, strokeStyle: colour });

                return note;
            });

            allNotes.push(...notes);

            // ---- Beams ----
            const beams = Beam.generateBeams(notes, {
                groups: beat.triplet ? [new Fraction(3, 3)] : [new Fraction(4, 4)],
            });

            for (const b of beams) {
                b.setStyle({ fillStyle: colour, strokeStyle: colour });
            }

            allBeams.push(...beams);

            // ---- Tuplets ----
            if (beat.triplet) {
                const tuplet = new Tuplet(notes, {
                    bracketed: !(
                        beat.data.length === 3 && beat.data.every((d) => d.type === 'rest')
                    ),
                    numNotes: 3,
                    notesOccupied: 2,
                });

                tuplet.setStyle({ fillStyle: colour, strokeStyle: colour });

                allTuplets.push(tuplet);
            }
        }

        Formatter.FormatAndDraw(context, stave, allNotes);

        for (const b of allBeams) {
            b.setContext(context).draw();
        }

        for (const t of allTuplets) {
            t.setContext(context).draw();
        }
    }
}
