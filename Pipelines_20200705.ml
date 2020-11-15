

let frameHeader = [| 0xFF; 0xd8 |].AsSpan()

let findStreamDelimiter (search:Span) (buffer:Span) : Nullable =
   let rec findHeaderArray (buffer:Span) (index:int) : int =
        if buffer.Length = 0 || buffer.Length < (search.Length - 1) then Nullable()
        elif buffer.[index] = search.[0] && buffer.Slice(1).SequenceEqual(search.Slice(1)) then
            Nullable (SequencePosition(buffer.GetPosition(index)))
        else findHeaderArray (buffer.Slice(1)) (index + 1)
   findHeaderArray buffer 0


let writePipe (stream : Stream) (writer : PipeWriter) =
    let rec writeLoop () = task {
        (* Allocate at least 1024 bytes from the PipeWriter *)
        let memory : Memory = writer.GetMemory(minimumBufferSize)
        try
            let! bytesRead = stream.ReadAsync(memory)
            if bytesRead = 0 then writer.Complete()
            else
                (* Tell the PipeWriter how much was read from the Socket *)
                writer.Advance(bytesRead)
        with
        | _ - writer.Complete()
        (* Make the data available to the PipeReader *)
        let! result = writer.FlushAsync()
        if result.IsCompleted then  writer.Complete()
        else return! writeLoop()
    }
    writeLoop ()


let readPipe (reader : PipeReader) (progress : IProgress)=
  let rec readLoop (reader : PipeReader) =
      let* result = reader.ReadAsync() in
      let buffer : ReadOnlySequence = result.Buffer in
      let* bufferAdvanced: readOnlySequence = findStreamDelimiter reader buffer in
      do
        reader.AdvanceTo(bufferAdvanced.Start, bufferAdvanced.End)
      if result.IsCompleted then
        reader.Complete()
      else return readLoop reader

  and findHeaderMarker (reader : PipeReader) (buffer : ReadOnlySequence) = task {
      let position = findStreamDelimiter buffer frameHeader
      if position.HasValue then
             progress.Report(buffer.Slice(0, position.Value))
             return! findHeaderMarker reader (buffer.Slice(buffer.GetPosition(1L, position.Value)))
      else return buffer }
  readLoop reader


let processFrame (request : HttpWebRequest) (progress : IProgress) = task {
    use! response = request.AsyncGetResponse()

    let frameBoundary =
        let contentType = response.Headers.["Content-Type"]
        let frameBoundary = contentType.AsSpan().Slice(contentType.IndexOf('=') + 1)

        if frameBoundary.StartsWith(boundyFrameDelimiter) then
            Encoding.UTF8.GetBytes(frameBoundary)
        else
            let frameBoundary = appendSpans boundyFrameDelimiter frameBoundary
            Encoding.UTF8.GetBytes(frameBoundary)

    use cameraStream = response.GetResponseStream()


    let pipe = new Pipe()

    let writing = writePipe cameraStream pipe.Writer
    let reading = readPipe pipe.Reader
    do! Task.WhenAll(reading, writing) }>


module type PipeReader = sig
  type 'a t

  val advance_to : consumed:int -> ?examined:int -> unit
  (*

    [reader.AdvanceTo(buffer.Start, buffer.End)]
    "I took nothing, but inspected everything - i.e. I can't make progress with that data"

    The examined data will be made available again in the following call?? This
    way the consumer does not have to buffer anything.

  *)
end
