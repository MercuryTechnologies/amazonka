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
-- Module      : Amazonka.Connect.Types.Attribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Attribute where

import Amazonka.Connect.Types.InstanceAttributeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A toggle for an individual feature at the instance level.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The value of the attribute.
    value :: Prelude.Maybe Prelude.Text,
    -- | The type of attribute.
    attributeType :: Prelude.Maybe InstanceAttributeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'attribute_value' - The value of the attribute.
--
-- 'attributeType', 'attribute_attributeType' - The type of attribute.
newAttribute ::
  Attribute
newAttribute =
  Attribute'
    { value = Prelude.Nothing,
      attributeType = Prelude.Nothing
    }

-- | The value of the attribute.
attribute_value :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_value = Lens.lens (\Attribute' {value} -> value) (\s@Attribute' {} a -> s {value = a} :: Attribute)

-- | The type of attribute.
attribute_attributeType :: Lens.Lens' Attribute (Prelude.Maybe InstanceAttributeType)
attribute_attributeType = Lens.lens (\Attribute' {attributeType} -> attributeType) (\s@Attribute' {} a -> s {attributeType = a} :: Attribute)

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "AttributeType")
      )

instance Prelude.Hashable Attribute where
  hashWithSalt salt' Attribute' {..} =
    salt' `Prelude.hashWithSalt` attributeType
      `Prelude.hashWithSalt` value

instance Prelude.NFData Attribute where
  rnf Attribute' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf attributeType
