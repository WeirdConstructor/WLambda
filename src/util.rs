// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!

  Implements utilitiy functions that are used by the WLambda
  standard library. For instance a SplitMix64 random number
  generator.

*/

//- splitmix64 (http://xoroshiro.di.unimi.it/splitmix64.c)
//"""
//  Written in 2015 by Sebastiano Vigna (vigna@acm.org)
//
//  To the extent possible under law, the author has dedicated all copyright
//  and related and neighboring rights to this software to the public domain
//  worldwide. This software is distributed without any warranty.
//
//  See <http://creativecommons.org/publicdomain/zero/1.0/>.
//"""
//
// Written by Alexander Stocko <as@coder.gg>
//
// To the extent possible under law, the author has dedicated all copyright
// and related and neighboring rights to this software to the public domain
// worldwide. This software is distributed without any warranty.
//
// See <LICENSE or http://creativecommons.org/publicdomain/zero/1.0/>

use std::cell::RefCell;
use std::num::Wrapping as w;

thread_local! {
    static RAND_STATE: RefCell<SplitMix64> =
        RefCell::new(SplitMix64::new(1_234_567_890));
}

pub fn srand(seed: i64) {
    RAND_STATE.with(|si| {
        *si.borrow_mut() = SplitMix64::new(seed as u64);
    })
}

pub fn rand_closed_open01() -> f64 {
    RAND_STATE.with(|si| u64_to_closed_open01(si.borrow_mut().next_u64()))
}

pub fn rand_i(max: u64) -> i64 {
    RAND_STATE.with(|si| (si.borrow_mut().next_u64() % max) as i64)
}

pub fn rand_full_i() -> i64 {
    RAND_STATE.with(|si| si.borrow_mut().next_i64())
}

/// The `SplitMix64` random number generator.
#[derive(Copy, Clone)]
pub struct SplitMix64(pub u64);

impl SplitMix64 {
    pub fn new(seed: u64) -> Self {
        Self(seed)
    }
    pub fn new_from_i64(seed: i64) -> Self {
        Self::new(u64::from_be_bytes(seed.to_be_bytes()))
    }

    #[inline]
    pub fn next_u64(&mut self) -> u64 {
        let mut z = w(self.0) + w(0x9E37_79B9_7F4A_7C15_u64);
        self.0 = z.0;
        z = (z ^ (z >> 30)) * w(0xBF58_476D_1CE4_E5B9_u64);
        z = (z ^ (z >> 27)) * w(0x94D0_49BB_1331_11EB_u64);
        (z ^ (z >> 31)).0
    }

    #[inline]
    pub fn next_i64(&mut self) -> i64 {
        i64::from_be_bytes(self.next_u64().to_be_bytes())
    }
}

// Taken from rand::distributions
// Licensed under the Apache License, Version 2.0
// Copyright 2018 Developers of the Rand project.
pub fn u64_to_open01(u: u64) -> f64 {
    use core::f64::EPSILON;
    let float_size = std::mem::size_of::<f64>() as u32 * 8;
    let fraction = u >> (float_size - 52);
    let exponent_bits: u64 = 1023_u64 << 52;
    f64::from_bits(fraction | exponent_bits) - (1.0 - EPSILON / 2.0)
}

// Taken from rand::distributions
// Licensed under the Apache License, Version 2.0
// Copyright 2018 Developers of the Rand project.
pub fn u64_to_closed_open01(u: u64) -> f64 {
    // Multiply-based method; 24/53 random bits; [0, 1) interval.
    // We use the most significant bits because for simple RNGs
    // those are usually more random.
    let float_size = std::mem::size_of::<f64>() as u32 * 8;
    let precision = 52 + 1;
    let scale = 1.0 / ((1_u64 << precision) as f64);

    let value = u >> (float_size - precision);
    scale * (value as f64)
}

// Taken from rand::distributions
// Licensed under the Apache License, Version 2.0
// Copyright 2018 Developers of the Rand project.
pub fn u64_to_open_closed01(u: u64) -> f64 {
    // Multiply-based method; 24/53 random bits; [0, 1) interval.
    // We use the most significant bits because for simple RNGs
    // those are usually more random.
    let float_size = std::mem::size_of::<f64>() as u32 * 8;
    let precision = 52 + 1;
    let scale = 1.0 / ((1_u64 << precision) as f64);

    let value = u >> (float_size - precision);
    scale * ((value + 1) as f64)
}

/// Original FNV Rust implementation taken from fnv crate on
/// crates.io by Alex Crichton <alex@alexcrichton.com>
/// under Apache-2.0 / MIT license Copyright 2018.
///
/// An implementation of the Fowler–Noll–Vo hash function.
///
/// See the [crate documentation](index.html) for more details.
#[derive(Copy, Clone, Debug)]
pub struct FnvHasher(u64);

impl Default for FnvHasher {
    #[inline]
    fn default() -> FnvHasher {
        FnvHasher(0xcbf2_9ce4_8422_2325)
    }
}

impl FnvHasher {
    #[inline]
    #[allow(dead_code)]
    pub fn with_key(key: u64) -> FnvHasher {
        FnvHasher(key)
    }

    #[inline]
    fn finish(self) -> u64 {
        self.0
    }

    #[inline]
    pub fn finish_i64(self) -> i64 {
        i64::from_be_bytes(self.finish().to_be_bytes())
    }

    #[inline]
    pub fn write_f64(&mut self, f: f64) {
        self.write(&f.to_bits().to_be_bytes());
    }

    #[inline]
    pub fn write_i64(&mut self, i: i64) {
        self.write(&i.to_be_bytes());
    }

    #[inline]
    #[allow(clippy::cast_lossless)]
    pub fn write(&mut self, bytes: &[u8]) {
        let FnvHasher(mut hash) = *self;

        for byte in bytes.iter() {
            hash ^= *byte as u64;
            hash = hash.wrapping_mul(0x0100_0000_01b3);
        }

        *self = FnvHasher(hash);
    }
}

pub fn now_timestamp() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs() as u64
}

pub fn rgba2hsvaf(rgba: (f64, f64, f64, f64)) -> (f64, f64, f64, f64) {
    let (h, s, v) = rgb2hsv(rgba.0, rgba.1, rgba.2);
    (h, s, v, rgba.3)
}

pub fn rgb2hsv(r: f64, g: f64, b: f64) -> (f64, f64, f64) {
    let c_max = r.max(g.max(b));
    let c_min = r.min(g.min(b));
    let delta = c_max - c_min;
    let mut hue = if delta < 0.000_001 {
        0.0
    } else if (r - c_max).abs() < 0.000_001 {
        60.0 * (((g - b) / delta) % 6.0)
    } else if (g - c_max).abs() < 0.000_001 {
        60.0 * (((b - r) / delta) + 2.0)
    } else {
        60.0 * (((r - g) / delta) + 4.0)
    };
    if hue < 0.0 {
        hue += 360.0
    }
    if hue > 360.0 {
        hue -= 360.0
    }
    let sat = if c_max < 0.000_001 { 0.0 } else { delta / c_max };
    let val = c_max;
    (hue, sat, val)
}

pub fn hsva2rgba(hsva: (f64, f64, f64, f64)) -> (f64, f64, f64, f64) {
    let (r, g, b) = hsv2rgb(hsva.0, hsva.1, hsva.2);
    (r, g, b, hsva.3)
}

pub fn hsv2rgb(hue: f64, sat: f64, val: f64) -> (f64, f64, f64) {
    let c = val * sat;
    let x = c * (1.0 - ((hue / 60.0) % 2.0 - 1.0).abs());
    let m = val - c;
    let (r_, g_, b_) = if (0.0..60.0).contains(&hue) {
        (c, x, 0.0)
    } else if (60.0..120.0).contains(&hue) {
        (x, c, 0.0)
    } else if (120.0..180.0).contains(&hue) {
        (0.0, c, x)
    } else if (180.0..240.0).contains(&hue) {
        (0.0, x, c)
    } else if (240.0..300.0).contains(&hue) {
        (x, 0.0, c)
    } else {
        // if hue >= 300.0 && hue < 360.0 {
        (c, 0.0, x)
    };
    //    println!("in: h={}, s={}, v={}, r:{}, g:{}, b: {}", hue, sat, val,
    //        (r_ + m) * 255.0,
    //        (g_ + m) * 255.0,
    //        (b_ + m) * 255.0);
    (r_ + m, g_ + m, b_ + m)
}

pub fn rgba2hexf(rgba: (f64, f64, f64, f64)) -> String {
    format!(
        "{:02x}{:02x}{:02x}{:02x}",
        (rgba.0 * 255.0).round() as u8,
        (rgba.1 * 255.0).round() as u8,
        (rgba.2 * 255.0).round() as u8,
        (rgba.3 * 255.0).round() as u8
    )
}

pub fn rgba2hex(rgba: (u8, u8, u8, u8)) -> String {
    format!("{:02x}{:02x}{:02x}{:02x}", rgba.0, rgba.1, rgba.2, rgba.3)
}

pub fn hex2hsvaf(s: &str) -> (f64, f64, f64, f64) {
    let (h, s, v, a) = hex2rgbaf(s);
    (h * 360.0, s * 100.0, v * 100.0, a * 100.0)
}

pub fn hsva2hexf(hsva: (f64, f64, f64, f64)) -> String {
    format!(
        "{:02x}{:02x}{:02x}{:02x}",
        ((hsva.0 / 360.0) * 255.0).round() as u8,
        (hsva.1 * 255.0).round() as u8,
        (hsva.2 * 255.0).round() as u8,
        (hsva.3 * 255.0).round() as u8
    )
}

pub fn hex2rgbaf(hex_str: &str) -> (f64, f64, f64, f64) {
    let (r, g, b, a) = hex2rgba(hex_str);
    (r as f64 / 255.0, g as f64 / 255.0, b as f64 / 255.0, a as f64 / 255.0)
}

fn shiftaddup4(u: u8) -> u8 {
    (u << 4) | u
}

pub fn hex2rgba(s: &str) -> (u8, u8, u8, u8) {
    match s.len() {
        8 => (
            u8::from_str_radix(&s[0..2], 16).unwrap_or(0),
            u8::from_str_radix(&s[2..4], 16).unwrap_or(0),
            u8::from_str_radix(&s[4..6], 16).unwrap_or(0),
            u8::from_str_radix(&s[6..8], 16).unwrap_or(255),
        ),
        6 => (
            u8::from_str_radix(&s[0..2], 16).unwrap_or(0),
            u8::from_str_radix(&s[2..4], 16).unwrap_or(0),
            u8::from_str_radix(&s[4..6], 16).unwrap_or(0),
            255,
        ),
        4 => (
            shiftaddup4(u8::from_str_radix(&s[0..1], 16).unwrap_or(0)),
            shiftaddup4(u8::from_str_radix(&s[1..2], 16).unwrap_or(0)),
            shiftaddup4(u8::from_str_radix(&s[2..3], 16).unwrap_or(0)),
            shiftaddup4(u8::from_str_radix(&s[3..4], 16).unwrap_or(0xFF)),
        ),
        3 => (
            shiftaddup4(u8::from_str_radix(&s[0..1], 16).unwrap_or(0)),
            shiftaddup4(u8::from_str_radix(&s[1..2], 16).unwrap_or(0)),
            shiftaddup4(u8::from_str_radix(&s[2..3], 16).unwrap_or(0)),
            255,
        ),
        2 => (
            shiftaddup4(u8::from_str_radix(&s[0..1], 16).unwrap_or(0)),
            shiftaddup4(u8::from_str_radix(&s[0..1], 16).unwrap_or(0)),
            shiftaddup4(u8::from_str_radix(&s[0..1], 16).unwrap_or(0)),
            255,
        ),
        _ => (255, 0, 255, 255),
    }
}

// Taken from https://github.com/febeling/edit-distance/blob/master/src/lib.rs
// Apache-2.0 License
/// Returns the edit distance between strings `a` and `b`.
///
/// The runtime complexity is `O(m*n)`, where `m` and `n` are the
/// strings' lengths.
#[allow(clippy::needless_range_loop)]
pub fn edit_distance(a: &str, b: &str) -> usize {
    let len_a = a.chars().count();
    let len_b = b.chars().count();
    if len_a < len_b {
        return edit_distance(b, a);
    }
    // handle special case of 0 length
    if len_a == 0 {
        return len_b;
    } else if len_b == 0 {
        return len_a;
    }

    let len_b = len_b + 1;

    let mut pre;
    let mut tmp;
    let mut cur = vec![0; len_b];

    // initialize string b
    for i in 1..len_b {
        cur[i] = i;
    }

    // calculate edit distance
    for (i, ca) in a.chars().enumerate() {
        // get first column for this row
        pre = cur[0];
        cur[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            tmp = cur[j + 1];
            cur[j + 1] = std::cmp::min(
                // deletion
                tmp + 1,
                std::cmp::min(
                    // insertion
                    cur[j] + 1,
                    // match or substitution
                    pre + if ca == cb { 0 } else { 1 },
                ),
            );
            pre = tmp;
        }
    }
    cur[len_b - 1]
}

// !build_phrase_matcher = {!(line, i) = @;
//    !j = i;
//    !phrase = $[];
//    {!(w, rline) = @;
//        if is_some[line.(j)] &and line.(j) == w {
//            .j += 1;
//            std:push phrase w;
//#            std:displayln "phrase" phrase;
//
//            return $t;
//        } {
//            if len[phrase] > 4 {
//                std:push found_phrases $[line, rline, phrase];
//                std:displayln "PHRASE" ~ std:str:join " " phrase;
//            };
//            .phrase = $[];
//            .j = i;
//            return $f;
//        }
//    }
//};
//
// !run_phrase_matcher_over_line = {!(pm, line) = @;
//    pm "" $[]; # reset it
//    iter i 0 => len[line] {
//        !j = i;
//        while pm[line.(j), line] {
//            .j += 1;
//        };
//    };
//};


struct PartMatcher<'a> {
    part: &'a [u64],
    min_len: usize,
    longest_match: usize,
}

impl<'a> PartMatcher<'a> {
    fn new(part: &'a [u64], min_len: usize) -> PartMatcher {
        Self {
            part,
            min_len,
            longest_match: 0,
        }
    }

    fn find_subsequence_in(&mut self, checked_part: &[u64]) -> Option<(usize, usize)> {
        let mut start_i = 0;
        let mut found_len = 0;
        let mut cur_i = 0;

        if checked_part.len() < self.min_len {
            return None;
        }

//        println!("FIND {:?} in {:?}", self.part, checked_part);
        for (part_i, elem) in checked_part.iter().enumerate() {
            if cur_i >= self.part.len() {
                break;
            }

            if self.part[cur_i] == *elem {
                if found_len == 0 {
                    start_i = part_i;
//            println!("MATCH cur_i={}, part_i={}", cur_i, part_i);
                }

                found_len += 1;
                cur_i += 1;
            } else if found_len > 0 {
                break;
            }
        }
//        println!("RES: pi={} found_len={}", part_i, found_len);

        if found_len >= self.min_len {
            if found_len > self.longest_match {
                self.longest_match = found_len;
            }

            Some((start_i, found_len))
        } else {
            None
        }
    }
}

pub struct AutoCorrSubseq {
    pub from_list_idx: usize,
    pub from_list_start_idx: usize,
    pub in_list_idx: usize,
    pub in_list_start_idx: usize,
    pub len: usize,
}

impl AutoCorrSubseq {
    fn in_list_matches(&self, list_idx: usize, idx: usize, len: usize) -> bool {
        return
            list_idx == self.in_list_idx
            && idx >= self.in_list_start_idx
            && (idx + len) <= self.in_list_start_idx + self.len;
    }
}

pub fn auto_correlate_lists(list_of_lists: &Vec<Vec<u64>>, min_len: usize) -> Vec<AutoCorrSubseq> {
    let mut subsequences : Vec<AutoCorrSubseq> = Vec::new();

    for list_i in 0..list_of_lists.len() {
        let cur_list = &list_of_lists[list_i];
//        println!("CURLIST: {:?}", cur_list);

        let mut i = 0;
        while i < cur_list.len() {
//            println!("SUBL: {:?}", &cur_list[i..]);
            if cur_list[i..].len() < min_len {
                break;
            }

            let mut pm = PartMatcher::new(&cur_list[i..], min_len);

            for next_list_i in (list_i + 1)..list_of_lists.len() {
                let mut subseq_i = 0;
                while subseq_i < list_of_lists[next_list_i].len() {
//                println!("list_i={} subseq_i={}: {:?}", next_list_i, subseq_i, &list_of_lists[next_list_i][subseq_i..]);

                    while let Some((find_i, find_len)) =
                        pm.find_subsequence_in(&list_of_lists[next_list_i][subseq_i..]) {
                        let find_i = subseq_i + find_i;

//                    println!("find_i={}, find_len={}", find_i, find_len);

                        let mut found = false;
                        for ss in subsequences.iter() {
                            if ss.in_list_matches(next_list_i, find_i, find_len) {
                                found = true;
                            }
                        }
                        if !found {
                            subsequences.push(AutoCorrSubseq {
                                from_list_idx: list_i,
                                from_list_start_idx: i,
                                in_list_idx: next_list_i,
                                in_list_start_idx: find_i,
                                len: find_len,
                            });
                        }
                        subseq_i = find_i + find_len;
//                        println!("new list_i={} subseq_i={}: {:?}", next_list_i, subseq_i, &list_of_lists[next_list_i][subseq_i..]);

                        if subseq_i >= list_of_lists[next_list_i].len() {
                            break;
                        }
                    }

                    subseq_i += 1;
                }
            }

            i += 1;
        }
    }
    subsequences
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn util_rgb2hsv() {
        assert_eq!(hsva2hexf(rgba2hsvaf(hex2rgbaf("FF0000FF"))), "00ffffff");
        assert_eq!(hsva2hexf(rgba2hsvaf(hex2rgbaf("00FF00FF"))), "55ffffff");
        assert_eq!(hsva2hexf(rgba2hsvaf(hex2rgbaf("0000FFFF"))), "aaffffff");
        assert_eq!(rgba2hsvaf(hex2rgbaf("FF00FFFF")), (300.0, 1.0, 1.0, 1.0));
        assert_eq!(rgba2hsvaf(hex2rgbaf("00FFFFFF")), (180.0, 1.0, 1.0, 1.0));
        assert_eq!(rgba2hsvaf(hex2rgbaf("FFFF00FF")), (60.0, 1.0, 1.0, 1.0));

        assert_eq!(rgba2hexf(hsva2rgba((300.0, 1.0, 1.0, 1.0))), "ff00ffff");
        assert_eq!(rgba2hexf(hsva2rgba((180.0, 1.0, 1.0, 1.0))), "00ffffff");
        assert_eq!(rgba2hexf(hsva2rgba((60.0, 1.0, 1.0, 1.0))), "ffff00ff");
        assert_eq!(rgba2hexf(hsva2rgba((100.0, 0.5, 0.25, 1.0))), "2b4020ff");
    }
}
