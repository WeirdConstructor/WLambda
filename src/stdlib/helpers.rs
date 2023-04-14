use crate::{threads::AVal, VVal};
use std::sync::{Condvar, Mutex};

#[allow(dead_code)]
pub struct PendingResult {
    lock: Mutex<(bool, AVal)>,
    cvar: Condvar,
}

#[allow(dead_code)]
impl PendingResult {
    pub fn new() -> Self {
        Self { lock: Mutex::new((true, AVal::None)), cvar: Condvar::new() }
    }

    pub fn send(&self, res: &VVal) -> Result<(), String> {
        match self.lock.lock() {
            Ok(mut pend) => {
                pend.0 = false;
                pend.1 = AVal::from_vval(&res);
                self.cvar.notify_one();
                Ok(())
            }
            Err(e) => Err(format!("PendingResult thread send error: {}", e)),
        }
    }

    pub fn wait(&self) -> Result<VVal, String> {
        let lock = match self.lock.lock() {
            Ok(lock) => lock,
            Err(e) => {
                return Err(format!("PendingResult thread lock error: {}", e));
            }
        };

        match self.cvar.wait_while(lock, |pend| pend.0) {
            Ok(pend) => Ok(pend.1.to_vval()),
            Err(e) => {
                return Err(format!("PendingResult thread wait error: {}", e));
            }
        }
    }
}

