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
-- Module      : Amazonka.DirectConnect.Types.RouterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.RouterType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the virtual router.
--
-- /See:/ 'newRouterType' smart constructor.
data RouterType = RouterType'
  { -- | The vendor for the virtual interface\'s router.
    vendor :: Prelude.Maybe Prelude.Text,
    -- | The virtual interface router platform.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The template for the virtual interface\'s router.
    xsltTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The router software.
    software :: Prelude.Maybe Prelude.Text,
    -- | The MAC Security (MACsec) template for the virtual interface\'s router.
    xsltTemplateNameForMacSec :: Prelude.Maybe Prelude.Text,
    -- | Identifies the router by a combination of vendor, platform, and software
    -- version. For example, @CiscoSystemsInc-2900SeriesRouters-IOS124@.
    routerTypeIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouterType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vendor', 'routerType_vendor' - The vendor for the virtual interface\'s router.
--
-- 'platform', 'routerType_platform' - The virtual interface router platform.
--
-- 'xsltTemplateName', 'routerType_xsltTemplateName' - The template for the virtual interface\'s router.
--
-- 'software', 'routerType_software' - The router software.
--
-- 'xsltTemplateNameForMacSec', 'routerType_xsltTemplateNameForMacSec' - The MAC Security (MACsec) template for the virtual interface\'s router.
--
-- 'routerTypeIdentifier', 'routerType_routerTypeIdentifier' - Identifies the router by a combination of vendor, platform, and software
-- version. For example, @CiscoSystemsInc-2900SeriesRouters-IOS124@.
newRouterType ::
  RouterType
newRouterType =
  RouterType'
    { vendor = Prelude.Nothing,
      platform = Prelude.Nothing,
      xsltTemplateName = Prelude.Nothing,
      software = Prelude.Nothing,
      xsltTemplateNameForMacSec = Prelude.Nothing,
      routerTypeIdentifier = Prelude.Nothing
    }

-- | The vendor for the virtual interface\'s router.
routerType_vendor :: Lens.Lens' RouterType (Prelude.Maybe Prelude.Text)
routerType_vendor = Lens.lens (\RouterType' {vendor} -> vendor) (\s@RouterType' {} a -> s {vendor = a} :: RouterType)

-- | The virtual interface router platform.
routerType_platform :: Lens.Lens' RouterType (Prelude.Maybe Prelude.Text)
routerType_platform = Lens.lens (\RouterType' {platform} -> platform) (\s@RouterType' {} a -> s {platform = a} :: RouterType)

-- | The template for the virtual interface\'s router.
routerType_xsltTemplateName :: Lens.Lens' RouterType (Prelude.Maybe Prelude.Text)
routerType_xsltTemplateName = Lens.lens (\RouterType' {xsltTemplateName} -> xsltTemplateName) (\s@RouterType' {} a -> s {xsltTemplateName = a} :: RouterType)

-- | The router software.
routerType_software :: Lens.Lens' RouterType (Prelude.Maybe Prelude.Text)
routerType_software = Lens.lens (\RouterType' {software} -> software) (\s@RouterType' {} a -> s {software = a} :: RouterType)

-- | The MAC Security (MACsec) template for the virtual interface\'s router.
routerType_xsltTemplateNameForMacSec :: Lens.Lens' RouterType (Prelude.Maybe Prelude.Text)
routerType_xsltTemplateNameForMacSec = Lens.lens (\RouterType' {xsltTemplateNameForMacSec} -> xsltTemplateNameForMacSec) (\s@RouterType' {} a -> s {xsltTemplateNameForMacSec = a} :: RouterType)

-- | Identifies the router by a combination of vendor, platform, and software
-- version. For example, @CiscoSystemsInc-2900SeriesRouters-IOS124@.
routerType_routerTypeIdentifier :: Lens.Lens' RouterType (Prelude.Maybe Prelude.Text)
routerType_routerTypeIdentifier = Lens.lens (\RouterType' {routerTypeIdentifier} -> routerTypeIdentifier) (\s@RouterType' {} a -> s {routerTypeIdentifier = a} :: RouterType)

instance Core.FromJSON RouterType where
  parseJSON =
    Core.withObject
      "RouterType"
      ( \x ->
          RouterType'
            Prelude.<$> (x Core..:? "vendor")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "xsltTemplateName")
            Prelude.<*> (x Core..:? "software")
            Prelude.<*> (x Core..:? "xsltTemplateNameForMacSec")
            Prelude.<*> (x Core..:? "routerTypeIdentifier")
      )

instance Prelude.Hashable RouterType where
  hashWithSalt salt' RouterType' {..} =
    salt' `Prelude.hashWithSalt` routerTypeIdentifier
      `Prelude.hashWithSalt` xsltTemplateNameForMacSec
      `Prelude.hashWithSalt` software
      `Prelude.hashWithSalt` xsltTemplateName
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` vendor

instance Prelude.NFData RouterType where
  rnf RouterType' {..} =
    Prelude.rnf vendor
      `Prelude.seq` Prelude.rnf routerTypeIdentifier
      `Prelude.seq` Prelude.rnf xsltTemplateNameForMacSec
      `Prelude.seq` Prelude.rnf software
      `Prelude.seq` Prelude.rnf xsltTemplateName
      `Prelude.seq` Prelude.rnf platform
