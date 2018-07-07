import numpy as np

# load f2py-based SUEWS calculation core
import slab

J = 1
ids = 1
ide = 5
jds = 1
jde = 5
kds = 1
kde = 5
ims = 1
ime = 5
jms = 1
jme = 5
kms = 1
kme = 5
its = 1
ite = 5
jts = 1
jte = 5
kts = 1
kte = 5
T1D = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 273.15
QV1D = np.random.rand(5).astype(np.float32)
P1D = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 80000.
FLHC = np.random.rand(5).astype(np.float32)
FLQC = np.random.rand(5).astype(np.float32)
PSFCPA = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 100000.
XLAND = np.ones(5).astype(np.float32)
TMN = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 273.15
HFX = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 100.0
QFX = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 100.0
TSK = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 273.15
QSFC = np.random.rand(5).astype(np.float32)
CHKLOWQ = np.random.rand(5).astype(np.float32)
LH = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 100.0
GSW = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 100.0
GLW = (np.random.rand(5).astype(np.float32)*0.1 + 1) * 100.0
CAPG = np.random.rand(5).astype(np.float32)
THC = np.random.rand(5).astype(np.float32)
SNOWC = np.zeros(5).astype(np.float32)
EMISS = np.random.rand(5).astype(np.float32)
MAVAIL = np.random.rand(5).astype(np.float32)
DELTSM = 60.
ROVCP = 0.5
XLV = 1.5
DTMIN = 1.
IFSNOW = 0
SVP1 = 10.
SVP2 = 11.
SVP3 = 12.
SVPT0 = 14.
EP2 = 15.
KARMAN = 0.41
EOMEG = 0.5
STBOLT = 5.67*10**(-8)
TSLB2D = np.array((np.random.rand(5, 5).astype(np.float32)*0.1 + 1) * 273.15,order='F')
ZS = np.arange(5, dtype = np.float32) + 0.5
DZS = np.ones(5).astype(np.float32)
num_soil_layers = 5
radiation = True
P1000mb = 1000.0


slab.module_sf_slab.slab1d(J,T1D,QV1D,P1D,FLHC,FLQC,            \
                   PSFCPA,XLAND,TMN,HFX,QFX,TSK,QSFC,CHKLOWQ,   \
                   LH,GSW,GLW,CAPG,THC,SNOWC,EMISS,MAVAIL,      \
                   DELTSM,ROVCP,XLV,DTMIN,IFSNOW,               \
                   SVP1,SVP2,SVP3,SVPT0,EP2,                    \
                   KARMAN,EOMEG,STBOLT,                         \
                   TSLB2D,ZS,DZS,num_soil_layers,radiation,     \
                   P1000mb,                                     \
                   ids,ide, jds,jde, kds,kde,                   \
                   ims,ime, jms,jme, kms,kme,                   \
                   its,ite, jts,jte, kts, kte)
