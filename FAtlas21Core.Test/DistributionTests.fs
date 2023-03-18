module FAtlas21Core.DistributionTests

open NUnit.Framework

open FAtlas.MathTypes
open FAtlas.Samples
open FAtlas.Distributions


[<Test>]
let testNormal () =
  let rng = new System.Random(101)
  let d01 = sample (normalDistn rng 0.0 1.0) 100000
  let m01 = sampleMean d01 id
  let v01 = unbiasedSampleVariance d01 id
  let s01 = sqrt v01
  Assert.AreEqual(0.0, m01, 0.01)
  Assert.AreEqual(1.0, s01, 0.1)

  ()

[<Test>]
let testNormalPair () =
  let rng = new System.Random(111)
  let s01_input = 1.2
  let s02_input = 1.4
  let rho_input = 0.8
  let d01 = sample (correlatedNormals rng s01_input s02_input rho_input) 10000
  let m01 = sampleMean d01 fst
  let m02 = sampleMean d01 snd
  let v01 = unbiasedSampleVariance d01 fst
  let v02 = unbiasedSampleVariance d01 snd
  let v12 = unbiasedSampleCovariance d01 fst snd
  let rho = v12 / sqrt(v01 * v02)
  Assert.AreEqual(s01_input, sqrt(v01), 0.01)
  Assert.AreEqual(s02_input, sqrt(v02), 0.01)
  Assert.AreEqual(rho_input, rho, 0.05)
  
  ()

