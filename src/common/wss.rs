use std::time::Instant;

pub struct WhySoSlow {
    pub startTime: Instant,
    pub readings: Vec<(Instant, &'static str)>
}

impl WhySoSlow {
    pub fn new(capacity: usize) -> Self {
        Self {
            startTime: Instant::now(),
            readings: Vec::with_capacity(capacity),
        }
    }

    #[inline(always)]
    pub fn record(&mut self, msg: &'static str) {
        self.readings.push((Instant::now(), msg))
    }

    #[inline(always)]
    pub fn dump(&self) {
        let mut lastTimestamp = self.startTime;

        for r in &self.readings {
            println!("reading {} took {}us", r.1, r.0.duration_since(lastTimestamp).as_micros());
            lastTimestamp = r.0;
        }

        println!("In total execution took {}us", Instant::now().duration_since(self.startTime).as_micros());
    }
}