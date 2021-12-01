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
-- Module      : Amazonka.GreengrassV2.Types.ComponentVersionListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentVersionListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component version in a list.
--
-- /See:/ 'newComponentVersionListItem' smart constructor.
data ComponentVersionListItem = ComponentVersionListItem'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentVersionListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'componentVersionListItem_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
--
-- 'componentVersion', 'componentVersionListItem_componentVersion' - The version of the component.
--
-- 'componentName', 'componentVersionListItem_componentName' - The name of the component.
newComponentVersionListItem ::
  ComponentVersionListItem
newComponentVersionListItem =
  ComponentVersionListItem'
    { arn = Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      componentName = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
componentVersionListItem_arn :: Lens.Lens' ComponentVersionListItem (Prelude.Maybe Prelude.Text)
componentVersionListItem_arn = Lens.lens (\ComponentVersionListItem' {arn} -> arn) (\s@ComponentVersionListItem' {} a -> s {arn = a} :: ComponentVersionListItem)

-- | The version of the component.
componentVersionListItem_componentVersion :: Lens.Lens' ComponentVersionListItem (Prelude.Maybe Prelude.Text)
componentVersionListItem_componentVersion = Lens.lens (\ComponentVersionListItem' {componentVersion} -> componentVersion) (\s@ComponentVersionListItem' {} a -> s {componentVersion = a} :: ComponentVersionListItem)

-- | The name of the component.
componentVersionListItem_componentName :: Lens.Lens' ComponentVersionListItem (Prelude.Maybe Prelude.Text)
componentVersionListItem_componentName = Lens.lens (\ComponentVersionListItem' {componentName} -> componentName) (\s@ComponentVersionListItem' {} a -> s {componentName = a} :: ComponentVersionListItem)

instance Core.FromJSON ComponentVersionListItem where
  parseJSON =
    Core.withObject
      "ComponentVersionListItem"
      ( \x ->
          ComponentVersionListItem'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "componentVersion")
            Prelude.<*> (x Core..:? "componentName")
      )

instance Prelude.Hashable ComponentVersionListItem where
  hashWithSalt salt' ComponentVersionListItem' {..} =
    salt' `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ComponentVersionListItem where
  rnf ComponentVersionListItem' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf componentVersion
