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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.ResourceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.ResourceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information about the resource.
--
-- /See:/ 'newResourceInfo' smart constructor.
data ResourceInfo = ResourceInfo'
  { -- | The Amazon resource name (ARN) that specifies the resource across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceInfo_arn' - The Amazon resource name (ARN) that specifies the resource across
-- services.
--
-- 'name', 'resourceInfo_name' - The name of the resource.
newResourceInfo ::
  ResourceInfo
newResourceInfo =
  ResourceInfo'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon resource name (ARN) that specifies the resource across
-- services.
resourceInfo_arn :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_arn = Lens.lens (\ResourceInfo' {arn} -> arn) (\s@ResourceInfo' {} a -> s {arn = a} :: ResourceInfo)

-- | The name of the resource.
resourceInfo_name :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_name = Lens.lens (\ResourceInfo' {name} -> name) (\s@ResourceInfo' {} a -> s {name = a} :: ResourceInfo)

instance Core.FromJSON ResourceInfo where
  parseJSON =
    Core.withObject
      "ResourceInfo"
      ( \x ->
          ResourceInfo'
            Prelude.<$> (x Core..:? "arn") Prelude.<*> (x Core..:? "name")
      )

instance Prelude.Hashable ResourceInfo where
  hashWithSalt salt' ResourceInfo' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ResourceInfo where
  rnf ResourceInfo' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf name
