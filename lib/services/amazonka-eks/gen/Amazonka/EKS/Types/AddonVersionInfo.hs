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
-- Module      : Amazonka.EKS.Types.AddonVersionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonVersionInfo where

import qualified Amazonka.Core as Core
import Amazonka.EKS.Types.Compatibility
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an add-on version.
--
-- /See:/ 'newAddonVersionInfo' smart constructor.
data AddonVersionInfo = AddonVersionInfo'
  { -- | The version of the add-on.
    addonVersion :: Prelude.Maybe Prelude.Text,
    -- | The architectures that the version supports.
    architecture :: Prelude.Maybe [Prelude.Text],
    -- | An object that represents the compatibilities of a version.
    compatibilities :: Prelude.Maybe [Compatibility]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddonVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonVersion', 'addonVersionInfo_addonVersion' - The version of the add-on.
--
-- 'architecture', 'addonVersionInfo_architecture' - The architectures that the version supports.
--
-- 'compatibilities', 'addonVersionInfo_compatibilities' - An object that represents the compatibilities of a version.
newAddonVersionInfo ::
  AddonVersionInfo
newAddonVersionInfo =
  AddonVersionInfo'
    { addonVersion = Prelude.Nothing,
      architecture = Prelude.Nothing,
      compatibilities = Prelude.Nothing
    }

-- | The version of the add-on.
addonVersionInfo_addonVersion :: Lens.Lens' AddonVersionInfo (Prelude.Maybe Prelude.Text)
addonVersionInfo_addonVersion = Lens.lens (\AddonVersionInfo' {addonVersion} -> addonVersion) (\s@AddonVersionInfo' {} a -> s {addonVersion = a} :: AddonVersionInfo)

-- | The architectures that the version supports.
addonVersionInfo_architecture :: Lens.Lens' AddonVersionInfo (Prelude.Maybe [Prelude.Text])
addonVersionInfo_architecture = Lens.lens (\AddonVersionInfo' {architecture} -> architecture) (\s@AddonVersionInfo' {} a -> s {architecture = a} :: AddonVersionInfo) Prelude.. Lens.mapping Lens.coerced

-- | An object that represents the compatibilities of a version.
addonVersionInfo_compatibilities :: Lens.Lens' AddonVersionInfo (Prelude.Maybe [Compatibility])
addonVersionInfo_compatibilities = Lens.lens (\AddonVersionInfo' {compatibilities} -> compatibilities) (\s@AddonVersionInfo' {} a -> s {compatibilities = a} :: AddonVersionInfo) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AddonVersionInfo where
  parseJSON =
    Core.withObject
      "AddonVersionInfo"
      ( \x ->
          AddonVersionInfo'
            Prelude.<$> (x Core..:? "addonVersion")
            Prelude.<*> (x Core..:? "architecture" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "compatibilities"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AddonVersionInfo where
  hashWithSalt salt' AddonVersionInfo' {..} =
    salt' `Prelude.hashWithSalt` compatibilities
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` addonVersion

instance Prelude.NFData AddonVersionInfo where
  rnf AddonVersionInfo' {..} =
    Prelude.rnf addonVersion
      `Prelude.seq` Prelude.rnf compatibilities
      `Prelude.seq` Prelude.rnf architecture
