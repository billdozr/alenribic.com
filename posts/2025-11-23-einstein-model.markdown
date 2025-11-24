---
title: Einstein Solid Heat Capacity on the HP-15C
author: Alen Ribic
date: November 23, 2025
tags: hp15c, physics, calculator
description: If you own an HP-15C, one of the greatest pocket scientific calculators ever built, you’re already holding a small numerical laboratory. In this post we’ll turn it into a statistical-mechanics engine.
---

If you own an HP-15C, one of the greatest pocket scientific calculators ever built, you’re already holding a small numerical laboratory. In this post we’ll turn it into a **statistical-mechanics engine**, capable of computing:

* the **Einstein solid heat capacity** $C_V(T)$
* the **change in internal energy** $\Delta U$
* the **change in entropy** $\Delta S$

…all from first principles, using the 15C’s built-in **programmable functions** and **definite integral** capability.

## 1. What Is the Einstein Solid Model?

Einstein proposed the first successful **quantum model of a crystalline solid**.
He modeled each atom as an independent 3D harmonic oscillator with quantized energy:

$$
E_n = \left(n + \frac12\right)\hbar \omega_E.
$$

This resolved the failure of classical physics, which predicted a constant heat capacity $C_V = 3R$ for all solids.

Experimentally, solids have:

* **low heat capacity at low temperature** (quantum regime)
* **saturation to $3R$ at high temperature** (Dulong–Petit limit)

The Einstein model captures this temperature dependence using a characteristic temperature.

## 2. The Einstein Temperature and the Ratio $x = \Theta_E/T$

The **Einstein temperature** is defined as:

$$
\Theta_E = \frac{\hbar \omega_E}{k_B}.
$$

This expresses the oscillator quantum $\hbar\omega_E$ on a temperature scale.

The key dimensionless parameter of the model is:

$$
x = \frac{\Theta_E}{T}.
$$

Why? Because

$$
x = \beta \hbar\omega_E = \frac{\hbar\omega_E}{k_B T} = \frac{\Theta_E}{T},
$$

which is the ratio of:

* **quantum vibrational energy** $\hbar\omega_E$
  to
* **thermal energy** $k_B T$.

Interpretation:

* **If $x \gg 1$** ($T \ll \Theta_E$): vibrations are frozen → $C_V \to 0$
* **If $x \ll 1$** ($T \gg \Theta_E$): oscillators behave classically → $C_V \to 3R$

The entire temperature dependence of the Einstein model is controlled by $x$.

## 3. Einstein Heat Capacity Formula

The molar heat capacity is:

$$
\frac{C_V}{R} = 3\frac{x^2 e^x}{(e^x - 1)^2}, \qquad x = \frac{\Theta_E}{T}.
$$

This is the expression we will program into the HP-15C.

## 4. Programming the HP-15C: Einstein Heat Capacity $C_V(T)/R$

We’ll write a program at **LBL A** that computes $C_V/R$ given:

* $\Theta_E$ stored in **R0**
* Temperature $T$ in **X**

### Store the Einstein temperature

Example: $\Theta_E = 230\ \text{K}$

```
230 STO 0
```

### Program: LBL A (computes $C_V/R$)

Enter program mode:
`g P/R`

Then enter:

```
f LBL A
RCL ÷ 0
1/x
g x²
g LASTx
e^x
×
g LASTx
1
-
g x²
÷
3
×
g RTN
```

### Using it

Compute $C_V/R$ at $T = 210$:

```
210
GSB A
```

Result for $\Theta_E = 230\ \text{K}$:

**2.717278**

## 5. Compute Internal Energy Change $\Delta U$ Using the Integrator

Thermodynamics gives:

$$
\Delta U = \int_{T_1}^{T_2} C_V(T), dT.
$$

Since `LBL A` returns $C_V/R$, the 15C computes:

$$
I = \int_{T_1}^{T_2} \frac{C_V}{R},dT = \frac{\Delta U}{R}.
$$

Multiply by $R \approx 8.314$ J/(mol·K) to obtain $\Delta U$.

## Wrapper function for integration (LBL 0)

Append:

```
f LBL 0
GSB A
g RTN
```

### Example: $\Delta U$ between 100 K and 300 K

```
100 ENTER 300
f ∫xy 0
8.314 ×
```

Expected (for $\Theta_E = 230$ K):
**$\Delta U \approx 4.34\times 10^3\ \text{J/mol}$**

## 6. Compute Entropy Change $\Delta S$

Entropy from heat capacity:

$$
\Delta S = \int_{T_1}^{T_2} \frac{C_V(T)}{T}, dT.
$$

Since `LBL A` returns $C_V/R$, we need to compute

$$
\frac{1}{T}\frac{C_V}{R}.
$$

## Wrapper function (LBL 1)

Append:

```
f LBL 1
STO 9
GSB A
RCL 9
÷
g RTN
```

This returns $(C_V/R)/T$.

### Example: $\Delta S$ between 100 K and 300 K

```
100 ENTER 300
f ∫xy 1
8.314 ×
```

Expected value:
**$\Delta S \approx 23\ \text{J/(mol·K)}$**

## 7. What You Just Built

Your HP-15C can now compute:

* Einstein heat capacity $C_V(T)$

* Internal energy change $\Delta U = \int C_V,dT$

* Entropy change $\Delta S = \int C_V/T,dT$

All directly from statistical mechanics and all on a pocket calculator that has stood the test of time.

***

[Edit: 2025-11-24] Thanks to u/Blue_Aluminium from r/calculators for suggesting a [cleaner, shorter version](https://www.reddit.com/r/calculators/comments/1p52clj/comment/nqkoi3v/) of the main subroutine.
