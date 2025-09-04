// types declared here correspond to the API exported in api_wasm.go

declare global {
    interface Window {
        CheckAndShowTypes: (program: string) => Promise<string>
        CompileAndShowGoOutput: (program: string) => Promise<{ types: string, goOutput: string }>
        InterpretGo: (program: string) => Promise<string>
    }
}

export {}
