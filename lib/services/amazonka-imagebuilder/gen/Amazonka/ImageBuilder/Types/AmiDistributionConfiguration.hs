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
-- Module      : Amazonka.ImageBuilder.Types.AmiDistributionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.AmiDistributionConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.LaunchPermissionConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Define and configure the output AMIs of the pipeline.
--
-- /See:/ 'newAmiDistributionConfiguration' smart constructor.
data AmiDistributionConfiguration = AmiDistributionConfiguration'
  { -- | Launch permissions can be used to configure which Amazon Web Services
    -- accounts can use the AMI to launch instances.
    launchPermission :: Prelude.Maybe LaunchPermissionConfiguration,
    -- | The ID of an account to which you want to distribute an image.
    targetAccountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The tags to apply to AMIs distributed to this Region.
    amiTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the output AMI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The KMS key identifier used to encrypt the distributed image.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The description of the distribution configuration. Minimum and maximum
    -- length are in characters.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmiDistributionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchPermission', 'amiDistributionConfiguration_launchPermission' - Launch permissions can be used to configure which Amazon Web Services
-- accounts can use the AMI to launch instances.
--
-- 'targetAccountIds', 'amiDistributionConfiguration_targetAccountIds' - The ID of an account to which you want to distribute an image.
--
-- 'amiTags', 'amiDistributionConfiguration_amiTags' - The tags to apply to AMIs distributed to this Region.
--
-- 'name', 'amiDistributionConfiguration_name' - The name of the output AMI.
--
-- 'kmsKeyId', 'amiDistributionConfiguration_kmsKeyId' - The KMS key identifier used to encrypt the distributed image.
--
-- 'description', 'amiDistributionConfiguration_description' - The description of the distribution configuration. Minimum and maximum
-- length are in characters.
newAmiDistributionConfiguration ::
  AmiDistributionConfiguration
newAmiDistributionConfiguration =
  AmiDistributionConfiguration'
    { launchPermission =
        Prelude.Nothing,
      targetAccountIds = Prelude.Nothing,
      amiTags = Prelude.Nothing,
      name = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Launch permissions can be used to configure which Amazon Web Services
-- accounts can use the AMI to launch instances.
amiDistributionConfiguration_launchPermission :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe LaunchPermissionConfiguration)
amiDistributionConfiguration_launchPermission = Lens.lens (\AmiDistributionConfiguration' {launchPermission} -> launchPermission) (\s@AmiDistributionConfiguration' {} a -> s {launchPermission = a} :: AmiDistributionConfiguration)

-- | The ID of an account to which you want to distribute an image.
amiDistributionConfiguration_targetAccountIds :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
amiDistributionConfiguration_targetAccountIds = Lens.lens (\AmiDistributionConfiguration' {targetAccountIds} -> targetAccountIds) (\s@AmiDistributionConfiguration' {} a -> s {targetAccountIds = a} :: AmiDistributionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply to AMIs distributed to this Region.
amiDistributionConfiguration_amiTags :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
amiDistributionConfiguration_amiTags = Lens.lens (\AmiDistributionConfiguration' {amiTags} -> amiTags) (\s@AmiDistributionConfiguration' {} a -> s {amiTags = a} :: AmiDistributionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the output AMI.
amiDistributionConfiguration_name :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe Prelude.Text)
amiDistributionConfiguration_name = Lens.lens (\AmiDistributionConfiguration' {name} -> name) (\s@AmiDistributionConfiguration' {} a -> s {name = a} :: AmiDistributionConfiguration)

-- | The KMS key identifier used to encrypt the distributed image.
amiDistributionConfiguration_kmsKeyId :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe Prelude.Text)
amiDistributionConfiguration_kmsKeyId = Lens.lens (\AmiDistributionConfiguration' {kmsKeyId} -> kmsKeyId) (\s@AmiDistributionConfiguration' {} a -> s {kmsKeyId = a} :: AmiDistributionConfiguration)

-- | The description of the distribution configuration. Minimum and maximum
-- length are in characters.
amiDistributionConfiguration_description :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe Prelude.Text)
amiDistributionConfiguration_description = Lens.lens (\AmiDistributionConfiguration' {description} -> description) (\s@AmiDistributionConfiguration' {} a -> s {description = a} :: AmiDistributionConfiguration)

instance Core.FromJSON AmiDistributionConfiguration where
  parseJSON =
    Core.withObject
      "AmiDistributionConfiguration"
      ( \x ->
          AmiDistributionConfiguration'
            Prelude.<$> (x Core..:? "launchPermission")
            Prelude.<*> (x Core..:? "targetAccountIds")
            Prelude.<*> (x Core..:? "amiTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..:? "description")
      )

instance
  Prelude.Hashable
    AmiDistributionConfiguration
  where
  hashWithSalt salt' AmiDistributionConfiguration' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` amiTags
      `Prelude.hashWithSalt` targetAccountIds
      `Prelude.hashWithSalt` launchPermission

instance Prelude.NFData AmiDistributionConfiguration where
  rnf AmiDistributionConfiguration' {..} =
    Prelude.rnf launchPermission
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf amiTags
      `Prelude.seq` Prelude.rnf targetAccountIds

instance Core.ToJSON AmiDistributionConfiguration where
  toJSON AmiDistributionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("launchPermission" Core..=)
              Prelude.<$> launchPermission,
            ("targetAccountIds" Core..=)
              Prelude.<$> targetAccountIds,
            ("amiTags" Core..=) Prelude.<$> amiTags,
            ("name" Core..=) Prelude.<$> name,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("description" Core..=) Prelude.<$> description
          ]
      )
