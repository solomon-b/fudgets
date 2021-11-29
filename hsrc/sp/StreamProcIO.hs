module StreamProcIO where
import SP

class StreamProcIO sp where -- or: SPIO SP_IO SpIO SpIo ?
  put :: o -> sp i o -> sp i o
  get :: (i -> sp i o) -> sp i o
  end :: sp i o -- null?

puts xs sp = foldr put sp xs

instance StreamProcIO SP where
  put = PutSP
  get = GetSP
  end = NullSP
