/// <reference types="vite/client" />

declare module "*.elm" {
  export const Elm: { [key: string]: ElmModule };

  interface ElmModule {
    init(options: { node?: HTMLElement; flags?: ElmData }): ElmApp;
  }

  type ElmData =
    | number
    | string
    | boolean
    | ElmData[]
    | null
    | { [key: string]: ElmData };

  interface ElmApp {
    ports: { [key: string]: ElmPort };
  }

  interface ElmPort {
    send(message: ElmData): void;
    subscribe<Data>(callback: (message: Data) => void): void;
    unsubscribe<Data>(callback: (message: Data) => void): void;
  }
}
