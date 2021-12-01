{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImageBuilder.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Image where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.ContainerRecipe
import Amazonka.ImageBuilder.Types.DistributionConfiguration
import Amazonka.ImageBuilder.Types.ImageRecipe
import Amazonka.ImageBuilder.Types.ImageState
import Amazonka.ImageBuilder.Types.ImageTestsConfiguration
import Amazonka.ImageBuilder.Types.ImageType
import Amazonka.ImageBuilder.Types.InfrastructureConfiguration
import Amazonka.ImageBuilder.Types.OutputResources
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An Image Builder image. You must specify exactly one recipe for the
-- image – either a container recipe (@containerRecipe@), which creates a
-- container image, or an image recipe (@imageRecipe@), which creates an
-- AMI.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The image recipe used when creating the image.
    imageRecipe :: Prelude.Maybe ImageRecipe,
    -- | The state of the image.
    state :: Prelude.Maybe ImageState,
    -- | The platform of the image.
    platform :: Prelude.Maybe Platform,
    -- | The image tests configuration used when creating this image.
    imageTestsConfiguration :: Prelude.Maybe ImageTestsConfiguration,
    -- | The Amazon Resource Name (ARN) of the image.
    --
    -- Semantic versioning is included in each object\'s Amazon Resource Name
    -- (ARN), at the level that applies to that object as follows:
    --
    -- 1.  Versionless ARNs and Name ARNs do not include specific values in any
    --     of the nodes. The nodes are either left off entirely, or they are
    --     specified as wildcards, for example: x.x.x.
    --
    -- 2.  Version ARNs have only the first three nodes:
    --     \<major>.\<minor>.\<patch>
    --
    -- 3.  Build version ARNs have all four nodes, and point to a specific
    --     build for a specific version of an object.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Collects additional information about the image being created, including
    -- the operating system (OS) version and package list. This information is
    -- used to enhance the overall experience of using EC2 Image Builder.
    -- Enabled by default.
    enhancedImageMetadataEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The date on which this image was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The recipe that is used to create an Image Builder container image.
    containerRecipe :: Prelude.Maybe ContainerRecipe,
    -- | The name of the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The infrastructure used when creating this image.
    infrastructureConfiguration :: Prelude.Maybe InfrastructureConfiguration,
    -- | The semantic version of the image.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Assignment:__ For the first three nodes you can assign any positive
    -- integer value, including zero, with an upper limit of 2^30-1, or
    -- 1073741823 for each node. Image Builder automatically assigns the build
    -- number to the fourth node.
    --
    -- __Patterns:__ You can use any numeric pattern that adheres to the
    -- assignment requirements for the nodes that you can assign. For example,
    -- you might choose a software version pattern, such as 1.0.0, or a date,
    -- such as 2021.01.01.
    --
    -- __Filtering:__ With semantic versioning, you have the flexibility to use
    -- wildcards (x) to specify the most recent versions or nodes when
    -- selecting the base image or components for your recipe. When you use a
    -- wildcard in any node, all nodes to the right of the first wildcard must
    -- also be wildcards.
    version :: Prelude.Maybe Prelude.Text,
    -- | The distribution configuration used when creating this image.
    distributionConfiguration :: Prelude.Maybe DistributionConfiguration,
    -- | The Amazon Resource Name (ARN) of the image pipeline that created this
    -- image.
    sourcePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this is an AMI or container image.
    type' :: Prelude.Maybe ImageType,
    -- | The name of the image pipeline that created this image.
    sourcePipelineName :: Prelude.Maybe Prelude.Text,
    -- | The output resources produced when creating this image.
    outputResources :: Prelude.Maybe OutputResources,
    -- | The operating system version of the instance. For example, Amazon Linux
    -- 2, Ubuntu 18, or Microsoft Windows Server 2019.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | The tags of the image.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageRecipe', 'image_imageRecipe' - The image recipe used when creating the image.
--
-- 'state', 'image_state' - The state of the image.
--
-- 'platform', 'image_platform' - The platform of the image.
--
-- 'imageTestsConfiguration', 'image_imageTestsConfiguration' - The image tests configuration used when creating this image.
--
-- 'arn', 'image_arn' - The Amazon Resource Name (ARN) of the image.
--
-- Semantic versioning is included in each object\'s Amazon Resource Name
-- (ARN), at the level that applies to that object as follows:
--
-- 1.  Versionless ARNs and Name ARNs do not include specific values in any
--     of the nodes. The nodes are either left off entirely, or they are
--     specified as wildcards, for example: x.x.x.
--
-- 2.  Version ARNs have only the first three nodes:
--     \<major>.\<minor>.\<patch>
--
-- 3.  Build version ARNs have all four nodes, and point to a specific
--     build for a specific version of an object.
--
-- 'enhancedImageMetadataEnabled', 'image_enhancedImageMetadataEnabled' - Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
--
-- 'dateCreated', 'image_dateCreated' - The date on which this image was created.
--
-- 'containerRecipe', 'image_containerRecipe' - The recipe that is used to create an Image Builder container image.
--
-- 'name', 'image_name' - The name of the image.
--
-- 'infrastructureConfiguration', 'image_infrastructureConfiguration' - The infrastructure used when creating this image.
--
-- 'version', 'image_version' - The semantic version of the image.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
--
-- 'distributionConfiguration', 'image_distributionConfiguration' - The distribution configuration used when creating this image.
--
-- 'sourcePipelineArn', 'image_sourcePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that created this
-- image.
--
-- 'type'', 'image_type' - Specifies whether this is an AMI or container image.
--
-- 'sourcePipelineName', 'image_sourcePipelineName' - The name of the image pipeline that created this image.
--
-- 'outputResources', 'image_outputResources' - The output resources produced when creating this image.
--
-- 'osVersion', 'image_osVersion' - The operating system version of the instance. For example, Amazon Linux
-- 2, Ubuntu 18, or Microsoft Windows Server 2019.
--
-- 'tags', 'image_tags' - The tags of the image.
newImage ::
  Image
newImage =
  Image'
    { imageRecipe = Prelude.Nothing,
      state = Prelude.Nothing,
      platform = Prelude.Nothing,
      imageTestsConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      enhancedImageMetadataEnabled = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      containerRecipe = Prelude.Nothing,
      name = Prelude.Nothing,
      infrastructureConfiguration = Prelude.Nothing,
      version = Prelude.Nothing,
      distributionConfiguration = Prelude.Nothing,
      sourcePipelineArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      sourcePipelineName = Prelude.Nothing,
      outputResources = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The image recipe used when creating the image.
image_imageRecipe :: Lens.Lens' Image (Prelude.Maybe ImageRecipe)
image_imageRecipe = Lens.lens (\Image' {imageRecipe} -> imageRecipe) (\s@Image' {} a -> s {imageRecipe = a} :: Image)

-- | The state of the image.
image_state :: Lens.Lens' Image (Prelude.Maybe ImageState)
image_state = Lens.lens (\Image' {state} -> state) (\s@Image' {} a -> s {state = a} :: Image)

-- | The platform of the image.
image_platform :: Lens.Lens' Image (Prelude.Maybe Platform)
image_platform = Lens.lens (\Image' {platform} -> platform) (\s@Image' {} a -> s {platform = a} :: Image)

-- | The image tests configuration used when creating this image.
image_imageTestsConfiguration :: Lens.Lens' Image (Prelude.Maybe ImageTestsConfiguration)
image_imageTestsConfiguration = Lens.lens (\Image' {imageTestsConfiguration} -> imageTestsConfiguration) (\s@Image' {} a -> s {imageTestsConfiguration = a} :: Image)

-- | The Amazon Resource Name (ARN) of the image.
--
-- Semantic versioning is included in each object\'s Amazon Resource Name
-- (ARN), at the level that applies to that object as follows:
--
-- 1.  Versionless ARNs and Name ARNs do not include specific values in any
--     of the nodes. The nodes are either left off entirely, or they are
--     specified as wildcards, for example: x.x.x.
--
-- 2.  Version ARNs have only the first three nodes:
--     \<major>.\<minor>.\<patch>
--
-- 3.  Build version ARNs have all four nodes, and point to a specific
--     build for a specific version of an object.
image_arn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_arn = Lens.lens (\Image' {arn} -> arn) (\s@Image' {} a -> s {arn = a} :: Image)

-- | Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
image_enhancedImageMetadataEnabled :: Lens.Lens' Image (Prelude.Maybe Prelude.Bool)
image_enhancedImageMetadataEnabled = Lens.lens (\Image' {enhancedImageMetadataEnabled} -> enhancedImageMetadataEnabled) (\s@Image' {} a -> s {enhancedImageMetadataEnabled = a} :: Image)

-- | The date on which this image was created.
image_dateCreated :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_dateCreated = Lens.lens (\Image' {dateCreated} -> dateCreated) (\s@Image' {} a -> s {dateCreated = a} :: Image)

-- | The recipe that is used to create an Image Builder container image.
image_containerRecipe :: Lens.Lens' Image (Prelude.Maybe ContainerRecipe)
image_containerRecipe = Lens.lens (\Image' {containerRecipe} -> containerRecipe) (\s@Image' {} a -> s {containerRecipe = a} :: Image)

-- | The name of the image.
image_name :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

-- | The infrastructure used when creating this image.
image_infrastructureConfiguration :: Lens.Lens' Image (Prelude.Maybe InfrastructureConfiguration)
image_infrastructureConfiguration = Lens.lens (\Image' {infrastructureConfiguration} -> infrastructureConfiguration) (\s@Image' {} a -> s {infrastructureConfiguration = a} :: Image)

-- | The semantic version of the image.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
image_version :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_version = Lens.lens (\Image' {version} -> version) (\s@Image' {} a -> s {version = a} :: Image)

-- | The distribution configuration used when creating this image.
image_distributionConfiguration :: Lens.Lens' Image (Prelude.Maybe DistributionConfiguration)
image_distributionConfiguration = Lens.lens (\Image' {distributionConfiguration} -> distributionConfiguration) (\s@Image' {} a -> s {distributionConfiguration = a} :: Image)

-- | The Amazon Resource Name (ARN) of the image pipeline that created this
-- image.
image_sourcePipelineArn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_sourcePipelineArn = Lens.lens (\Image' {sourcePipelineArn} -> sourcePipelineArn) (\s@Image' {} a -> s {sourcePipelineArn = a} :: Image)

-- | Specifies whether this is an AMI or container image.
image_type :: Lens.Lens' Image (Prelude.Maybe ImageType)
image_type = Lens.lens (\Image' {type'} -> type') (\s@Image' {} a -> s {type' = a} :: Image)

-- | The name of the image pipeline that created this image.
image_sourcePipelineName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_sourcePipelineName = Lens.lens (\Image' {sourcePipelineName} -> sourcePipelineName) (\s@Image' {} a -> s {sourcePipelineName = a} :: Image)

-- | The output resources produced when creating this image.
image_outputResources :: Lens.Lens' Image (Prelude.Maybe OutputResources)
image_outputResources = Lens.lens (\Image' {outputResources} -> outputResources) (\s@Image' {} a -> s {outputResources = a} :: Image)

-- | The operating system version of the instance. For example, Amazon Linux
-- 2, Ubuntu 18, or Microsoft Windows Server 2019.
image_osVersion :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_osVersion = Lens.lens (\Image' {osVersion} -> osVersion) (\s@Image' {} a -> s {osVersion = a} :: Image)

-- | The tags of the image.
image_tags :: Lens.Lens' Image (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
image_tags = Lens.lens (\Image' {tags} -> tags) (\s@Image' {} a -> s {tags = a} :: Image) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Core..:? "imageRecipe")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "imageTestsConfiguration")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "enhancedImageMetadataEnabled")
            Prelude.<*> (x Core..:? "dateCreated")
            Prelude.<*> (x Core..:? "containerRecipe")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "infrastructureConfiguration")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "distributionConfiguration")
            Prelude.<*> (x Core..:? "sourcePipelineArn")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "sourcePipelineName")
            Prelude.<*> (x Core..:? "outputResources")
            Prelude.<*> (x Core..:? "osVersion")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Image where
  hashWithSalt salt' Image' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` outputResources
      `Prelude.hashWithSalt` sourcePipelineName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` sourcePipelineArn
      `Prelude.hashWithSalt` distributionConfiguration
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` infrastructureConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` containerRecipe
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` enhancedImageMetadataEnabled
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` imageTestsConfiguration
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` imageRecipe

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf imageRecipe
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf outputResources
      `Prelude.seq` Prelude.rnf sourcePipelineName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf sourcePipelineArn
      `Prelude.seq` Prelude.rnf distributionConfiguration
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf infrastructureConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf containerRecipe
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf enhancedImageMetadataEnabled
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf imageTestsConfiguration
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf state
