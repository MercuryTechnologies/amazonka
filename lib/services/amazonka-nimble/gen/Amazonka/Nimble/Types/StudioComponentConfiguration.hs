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
-- Module      : Amazonka.Nimble.Types.StudioComponentConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.ActiveDirectoryConfiguration
import Amazonka.Nimble.Types.ComputeFarmConfiguration
import Amazonka.Nimble.Types.LicenseServiceConfiguration
import Amazonka.Nimble.Types.SharedFileSystemConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the studio component, based on component type.
--
-- /See:/ 'newStudioComponentConfiguration' smart constructor.
data StudioComponentConfiguration = StudioComponentConfiguration'
  { -- | The configuration for a Microsoft Active Directory (Microsoft AD) studio
    -- resource.
    activeDirectoryConfiguration :: Prelude.Maybe ActiveDirectoryConfiguration,
    -- | The configuration for a license service that is associated with a studio
    -- resource.
    licenseServiceConfiguration :: Prelude.Maybe LicenseServiceConfiguration,
    -- | The configuration for a shared file storage system that is associated
    -- with a studio resource.
    sharedFileSystemConfiguration :: Prelude.Maybe SharedFileSystemConfiguration,
    -- | The configuration for a render farm that is associated with a studio
    -- resource.
    computeFarmConfiguration :: Prelude.Maybe ComputeFarmConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioComponentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryConfiguration', 'studioComponentConfiguration_activeDirectoryConfiguration' - The configuration for a Microsoft Active Directory (Microsoft AD) studio
-- resource.
--
-- 'licenseServiceConfiguration', 'studioComponentConfiguration_licenseServiceConfiguration' - The configuration for a license service that is associated with a studio
-- resource.
--
-- 'sharedFileSystemConfiguration', 'studioComponentConfiguration_sharedFileSystemConfiguration' - The configuration for a shared file storage system that is associated
-- with a studio resource.
--
-- 'computeFarmConfiguration', 'studioComponentConfiguration_computeFarmConfiguration' - The configuration for a render farm that is associated with a studio
-- resource.
newStudioComponentConfiguration ::
  StudioComponentConfiguration
newStudioComponentConfiguration =
  StudioComponentConfiguration'
    { activeDirectoryConfiguration =
        Prelude.Nothing,
      licenseServiceConfiguration = Prelude.Nothing,
      sharedFileSystemConfiguration =
        Prelude.Nothing,
      computeFarmConfiguration = Prelude.Nothing
    }

-- | The configuration for a Microsoft Active Directory (Microsoft AD) studio
-- resource.
studioComponentConfiguration_activeDirectoryConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe ActiveDirectoryConfiguration)
studioComponentConfiguration_activeDirectoryConfiguration = Lens.lens (\StudioComponentConfiguration' {activeDirectoryConfiguration} -> activeDirectoryConfiguration) (\s@StudioComponentConfiguration' {} a -> s {activeDirectoryConfiguration = a} :: StudioComponentConfiguration)

-- | The configuration for a license service that is associated with a studio
-- resource.
studioComponentConfiguration_licenseServiceConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe LicenseServiceConfiguration)
studioComponentConfiguration_licenseServiceConfiguration = Lens.lens (\StudioComponentConfiguration' {licenseServiceConfiguration} -> licenseServiceConfiguration) (\s@StudioComponentConfiguration' {} a -> s {licenseServiceConfiguration = a} :: StudioComponentConfiguration)

-- | The configuration for a shared file storage system that is associated
-- with a studio resource.
studioComponentConfiguration_sharedFileSystemConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe SharedFileSystemConfiguration)
studioComponentConfiguration_sharedFileSystemConfiguration = Lens.lens (\StudioComponentConfiguration' {sharedFileSystemConfiguration} -> sharedFileSystemConfiguration) (\s@StudioComponentConfiguration' {} a -> s {sharedFileSystemConfiguration = a} :: StudioComponentConfiguration)

-- | The configuration for a render farm that is associated with a studio
-- resource.
studioComponentConfiguration_computeFarmConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe ComputeFarmConfiguration)
studioComponentConfiguration_computeFarmConfiguration = Lens.lens (\StudioComponentConfiguration' {computeFarmConfiguration} -> computeFarmConfiguration) (\s@StudioComponentConfiguration' {} a -> s {computeFarmConfiguration = a} :: StudioComponentConfiguration)

instance Core.FromJSON StudioComponentConfiguration where
  parseJSON =
    Core.withObject
      "StudioComponentConfiguration"
      ( \x ->
          StudioComponentConfiguration'
            Prelude.<$> (x Core..:? "activeDirectoryConfiguration")
            Prelude.<*> (x Core..:? "licenseServiceConfiguration")
            Prelude.<*> (x Core..:? "sharedFileSystemConfiguration")
            Prelude.<*> (x Core..:? "computeFarmConfiguration")
      )

instance
  Prelude.Hashable
    StudioComponentConfiguration
  where
  hashWithSalt salt' StudioComponentConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` computeFarmConfiguration
      `Prelude.hashWithSalt` sharedFileSystemConfiguration
      `Prelude.hashWithSalt` licenseServiceConfiguration
      `Prelude.hashWithSalt` activeDirectoryConfiguration

instance Prelude.NFData StudioComponentConfiguration where
  rnf StudioComponentConfiguration' {..} =
    Prelude.rnf activeDirectoryConfiguration
      `Prelude.seq` Prelude.rnf computeFarmConfiguration
      `Prelude.seq` Prelude.rnf sharedFileSystemConfiguration
      `Prelude.seq` Prelude.rnf licenseServiceConfiguration

instance Core.ToJSON StudioComponentConfiguration where
  toJSON StudioComponentConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("activeDirectoryConfiguration" Core..=)
              Prelude.<$> activeDirectoryConfiguration,
            ("licenseServiceConfiguration" Core..=)
              Prelude.<$> licenseServiceConfiguration,
            ("sharedFileSystemConfiguration" Core..=)
              Prelude.<$> sharedFileSystemConfiguration,
            ("computeFarmConfiguration" Core..=)
              Prelude.<$> computeFarmConfiguration
          ]
      )
