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
-- Module      : Amazonka.SSMIncidents.Types.AttributeValueList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.AttributeValueList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use the AttributeValueList to filter by string or integer values.
--
-- /See:/ 'newAttributeValueList' smart constructor.
data AttributeValueList = AttributeValueList'
  { -- | The list of string values that the filter matches.
    stringValues :: Prelude.Maybe [Prelude.Text],
    -- | The list of integer values that the filter matches.
    integerValues :: Prelude.Maybe [Prelude.Int]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeValueList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringValues', 'attributeValueList_stringValues' - The list of string values that the filter matches.
--
-- 'integerValues', 'attributeValueList_integerValues' - The list of integer values that the filter matches.
newAttributeValueList ::
  AttributeValueList
newAttributeValueList =
  AttributeValueList'
    { stringValues = Prelude.Nothing,
      integerValues = Prelude.Nothing
    }

-- | The list of string values that the filter matches.
attributeValueList_stringValues :: Lens.Lens' AttributeValueList (Prelude.Maybe [Prelude.Text])
attributeValueList_stringValues = Lens.lens (\AttributeValueList' {stringValues} -> stringValues) (\s@AttributeValueList' {} a -> s {stringValues = a} :: AttributeValueList) Prelude.. Lens.mapping Lens.coerced

-- | The list of integer values that the filter matches.
attributeValueList_integerValues :: Lens.Lens' AttributeValueList (Prelude.Maybe [Prelude.Int])
attributeValueList_integerValues = Lens.lens (\AttributeValueList' {integerValues} -> integerValues) (\s@AttributeValueList' {} a -> s {integerValues = a} :: AttributeValueList) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AttributeValueList where
  hashWithSalt salt' AttributeValueList' {..} =
    salt' `Prelude.hashWithSalt` integerValues
      `Prelude.hashWithSalt` stringValues

instance Prelude.NFData AttributeValueList where
  rnf AttributeValueList' {..} =
    Prelude.rnf stringValues
      `Prelude.seq` Prelude.rnf integerValues

instance Core.ToJSON AttributeValueList where
  toJSON AttributeValueList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stringValues" Core..=) Prelude.<$> stringValues,
            ("integerValues" Core..=) Prelude.<$> integerValues
          ]
      )
