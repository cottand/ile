// types declared here correspond to the API exported in api_wasm.go

declare global {
    export interface Window {
        CheckAndShowTypes: (program: string) => string
        CompileAndShowGoOutput: (program: string) => ({ error: string } | { types: string, goOutput: string })
        InterpretGo: (program: string) => Promise<string>
    }
}
