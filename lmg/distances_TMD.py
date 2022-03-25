import tmd
import tmd.view as view
from matplotlib import cm
import numpy as np


pop1 = tmd.io.load_population("isotropic_3D_swcs")
pop2 = tmd.io.load_population("isotropic_2D_swcs")

phs1 = [tmd.methods.get_ph_neuron(n, neurite_type='dendrites') for n in pop1.neurons]
phs2 = [tmd.methods.get_ph_neuron(n, neurite_type='dendrites') for n in pop2.neurons]

# Normalize the limits
xlims, ylims = tmd.analysis.get_limits(phs1 + phs2)

# Create average images for populations
imgs1 = [tmd.analysis.get_persistence_image_data(p, xlims=xlims, ylims=ylims) for p in phs1]
IMG1 = tmd.analysis.get_average_persistence_image(phs1, xlims=xlims, ylims=ylims)
imgs2 = [tmd.analysis.get_persistence_image_data(p, xlims=xlims, ylims=ylims) for p in phs2]
IMG2 = tmd.analysis.get_average_persistence_image(phs2, xlims=xlims, ylims=ylims)

# You can plot the images if you want to create pretty figures
average_figure1 = view.common.plot_img_basic(IMG1, title='', xlims=xlims, ylims=ylims, cmap=cm.jet)
average_figure2 = view.common.plot_img_basic(IMG2, title='', xlims=xlims, ylims=ylims, cmap=cm.jet)

# Create the diffence between the two images
DIMG = tmd.analysis.get_image_diff_data(IMG1, IMG2) # subtracts IMG2 from IMG1 so anything red IMG1 has more of it and anything blue IMG2 has more of it - or that's how it is supposed to be :)

# Plot the difference between them
diff_image = view.common.plot_img_basic(DIMG, vmin=-1.0, vmax=1.0) # vmin, vmax important to see changes
# Quantify the absolute distance between the two averages
dist = np.sum(np.abs(DIMG))



import itertools
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import os
import tmd
import tmd.view as view
from matplotlib import cm
import numpy as np

metadata = pd.read_csv("metadatags.csv")
# smetadata = metadata.sort_values(by=['paths'],kind='heapsort')
fnames = sorted(os.listdir("isotropic_swcs"))
sorterIndex = dict(zip(fnames, range(len(fnames))))
metadata['fn_Rank'] = metadata['paths'].map(sorterIndex)
metadata.sort_values('fn_Rank',ascending = True, inplace = True)
metadata.reset_index(inplace=True)

pop = tmd.io.load_population("isotropic_swcs")
phs = [tmd.methods.get_ph_neuron(n, neurite_type='dendrites') for n in pop.neurons]
xlims, ylims = tmd.analysis.get_limits(phs)
imgs = [tmd.analysis.get_persistence_image_data(p, xlims=xlims, ylims=ylims) for p in phs]
dArray = np.zeros((161, 161))
for i in range(0,161):
    for j in range(0,161):
        # dArray[j,i] = np.sum(np.abs(tmd.analysis.get_image_diff_data(imgs[i], imgs[j])))
        dArray[j,i] = tmd.analysis.distance_horizontal(phs[i], phs[j])
        
# dimgs = [np.sum(np.abs(tmd.analysis.get_image_diff_data(img1, img2))) for img1 in imgs for img2 in imgs]# subtracts IMG2 from IMG1 so anything red IMG1 has more of it and anything blue IMG2 has more of it - or that's how it is supposed to be :)
# dArray = np.reshape(dimgs,(161,161))
ddf = pd.DataFrame(dArray,columns=(metadata.Cell_type.astype(str) + " " + metadata.ids.astype(str) + " " + metadata.Planar.astype(str)),index=(metadata.Model_organism.astype(str)  + ' ' + metadata.ids.astype(str) + " " + metadata.Planar.astype(str)))
# ddf.replace([np.inf, -np.inf], 0, inplace=True)
ax = sns.clustermap(ddf,cmap="mako",figsize=(25,25),xticklabels=1,yticklabels=1)
ax.savefig('TMD_clustermap.pdf',dpi=300)
# plt.setp(ax.ax_heatmap.get_yticklabels(), rotation=0)  # For y axis
# plt.setp(ax.ax_heatmap.get_xticklabels(), rotation=90) # For x axis
# plt.show()
ddf = pd.DataFrame(dArray,columns=(metadata.ids.astype(str)),index=(metadata.ids.astype(str)))
ddf.to_csv('TMD_dists.csv')
treecol = 'b'
for i in range(0,161):
    dgfig = view.plot.diagram(phs[i], color=treecol)
    view.common.save_plot(imgs[i],output_path='TMD_diagrams/', prefile=metadata.ids[i].astype(str))
    dgfig = view.plot.barcode(phs[i], color=treecol)
    view.common.save_plot(imgs[i],output_path='TMD_barcodes/', prefile=metadata.ids[i].astype(str))
    imfig = view.plot.persistence_image(phs[i], xlims=xlims, ylims=ylims)
    view.common.save_plot(imgs[i],output_path='TMD_imgs/', prefile=metadata.ids[i].astype(str))

# min_max_size = np.min([len(p) for p in ph_list])
# intervals = np.linspace(step_size, min_max_size, (min_max_size-step_size)/step_size)

# distancesCD4 = []
# distancesCD4std = []
# distancesCD48 = []
# distancesCD48std = []

# for i in intervals:

#     d4 = []
#     d48 = []

#     for s in np.arange(samples):
#         Zns = []
#         for ph in ph_list:
#             ph_random_indices = random.choice(np.arange(len(ph)), int(i), replace=False)
#             ph_random = np.array(ph)[ph_random_indices]
#             Z = average_weighted_ph_from_list(ph_random, norm_factor=None, xlims=xlims, ylims=ylims)
#             Zns.append(Z)

#         DiffCD4 = np.sum(np.abs(img_diff(Zns[0], Zns[1], norm=False)))
#         DiffCD48 = np.sum(np.abs(img_diff(Zns[0], Zns[2], norm=False)))

#         d4.append(DiffCD4/ (DiffCD4+DiffCD48))
#         d48.append(DiffCD48/ (DiffCD4+DiffCD48))

#     distancesCD4.append(np.mean(d4))
#     distancesCD48.append(np.mean(d48))
#     distancesCD4std.append(np.std(d4))
#     distancesCD48std.append(np.std(d48))

# fig = plt.figure(figsize=(15,10))
# plt.plot(intervals, distancesCD4, c='b', label='Dist: C-D4')
# plt.plot(intervals, distancesCD48, c='r', label='Dist: C-D48')

# plt.fill_between(intervals, np.subtract(distancesCD4, distancesCD4std), np.add(distancesCD4, distancesCD4std), color='b', alpha=0.2)
# plt.fill_between(intervals, np.subtract(distancesCD48, distancesCD48std), np.add(distancesCD48, distancesCD48std), color='r', alpha=0.2)

# plt.xlabel('Number of cells', fontsize=18)
# plt.ylabel('Normalized distance', fontsize=18)
# plt.legend()
# plt.title(title)




def neu_all(neuron, plane='xy', feature='radial_distances', title='',
            diameter=True, treecol='b', xlims=None, ylims=None, neurite_type='basal', **kwargs):
    '''Subplot with ph, barcode
       and tree within spheres
    '''
    from tmd import utils as _utils
    from matplotlib.collections import LineCollection

    kwargs['output_path'] = kwargs.get('output_path', None)

    fig1, ax1 = view.view.neuron(neuron, new_fig=True, subplot=221, plane='xy', neurite_type=[neurite_type],
                             title=title, treecolor=treecol, diameter=diameter)

    if plane in ['xy', 'yx', 'zx', 'xz', 'yz', 'zy']:
        ph = tmd.Topology.methods.get_ph_neuron(neuron, feature=feature, neurite_type=neurite_type)
    else:
        raise Exception('Plane value not recognised')

    bounds = max(max(ph))
    fig1, ax2 = view.plot.diagram(ph, new_fig=False, subplot=222, color=treecol)
    fig1, ax3 = view.plot.barcode(ph, new_fig=False, subplot=223, color=treecol)
    fig1, ax4 = view.plot.persistence_image(ph, new_fig=False, subplot=224, xlims=xlims, ylims=ylims)
    view.common.plt.tight_layout(True)

    if kwargs['output_path'] is not None:
        fig = view.common.plt.save_plot(fig=ax1, **kwargs)

    return fig1, ax1

