// types declared here correspond to the API exported in api_wasm.go

declare global {
    export interface Window {
        CheckAndShowTypes: (program: string) => string
    }
}
