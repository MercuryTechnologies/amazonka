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
-- Module      : Amazonka.LicenseManager.Types.ResourceInventory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ResourceInventory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Details about a resource.
--
-- /See:/ 'newResourceInventory' smart constructor.
data ResourceInventory = ResourceInventory'
  { -- | Platform of the resource.
    platform :: Prelude.Maybe Prelude.Text,
    -- | ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | Type of resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Platform version of the resource in the inventory.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | ID of the account that owns the resource.
    resourceOwningAccountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceInventory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'resourceInventory_platform' - Platform of the resource.
--
-- 'resourceId', 'resourceInventory_resourceId' - ID of the resource.
--
-- 'resourceType', 'resourceInventory_resourceType' - Type of resource.
--
-- 'platformVersion', 'resourceInventory_platformVersion' - Platform version of the resource in the inventory.
--
-- 'resourceArn', 'resourceInventory_resourceArn' - Amazon Resource Name (ARN) of the resource.
--
-- 'resourceOwningAccountId', 'resourceInventory_resourceOwningAccountId' - ID of the account that owns the resource.
newResourceInventory ::
  ResourceInventory
newResourceInventory =
  ResourceInventory'
    { platform = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceOwningAccountId = Prelude.Nothing
    }

-- | Platform of the resource.
resourceInventory_platform :: Lens.Lens' ResourceInventory (Prelude.Maybe Prelude.Text)
resourceInventory_platform = Lens.lens (\ResourceInventory' {platform} -> platform) (\s@ResourceInventory' {} a -> s {platform = a} :: ResourceInventory)

-- | ID of the resource.
resourceInventory_resourceId :: Lens.Lens' ResourceInventory (Prelude.Maybe Prelude.Text)
resourceInventory_resourceId = Lens.lens (\ResourceInventory' {resourceId} -> resourceId) (\s@ResourceInventory' {} a -> s {resourceId = a} :: ResourceInventory)

-- | Type of resource.
resourceInventory_resourceType :: Lens.Lens' ResourceInventory (Prelude.Maybe ResourceType)
resourceInventory_resourceType = Lens.lens (\ResourceInventory' {resourceType} -> resourceType) (\s@ResourceInventory' {} a -> s {resourceType = a} :: ResourceInventory)

-- | Platform version of the resource in the inventory.
resourceInventory_platformVersion :: Lens.Lens' ResourceInventory (Prelude.Maybe Prelude.Text)
resourceInventory_platformVersion = Lens.lens (\ResourceInventory' {platformVersion} -> platformVersion) (\s@ResourceInventory' {} a -> s {platformVersion = a} :: ResourceInventory)

-- | Amazon Resource Name (ARN) of the resource.
resourceInventory_resourceArn :: Lens.Lens' ResourceInventory (Prelude.Maybe Prelude.Text)
resourceInventory_resourceArn = Lens.lens (\ResourceInventory' {resourceArn} -> resourceArn) (\s@ResourceInventory' {} a -> s {resourceArn = a} :: ResourceInventory)

-- | ID of the account that owns the resource.
resourceInventory_resourceOwningAccountId :: Lens.Lens' ResourceInventory (Prelude.Maybe Prelude.Text)
resourceInventory_resourceOwningAccountId = Lens.lens (\ResourceInventory' {resourceOwningAccountId} -> resourceOwningAccountId) (\s@ResourceInventory' {} a -> s {resourceOwningAccountId = a} :: ResourceInventory)

instance Core.FromJSON ResourceInventory where
  parseJSON =
    Core.withObject
      "ResourceInventory"
      ( \x ->
          ResourceInventory'
            Prelude.<$> (x Core..:? "Platform")
            Prelude.<*> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "PlatformVersion")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "ResourceOwningAccountId")
      )

instance Prelude.Hashable ResourceInventory where
  hashWithSalt salt' ResourceInventory' {..} =
    salt'
      `Prelude.hashWithSalt` resourceOwningAccountId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` platform

instance Prelude.NFData ResourceInventory where
  rnf ResourceInventory' {..} =
    Prelude.rnf platform
      `Prelude.seq` Prelude.rnf resourceOwningAccountId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceId
