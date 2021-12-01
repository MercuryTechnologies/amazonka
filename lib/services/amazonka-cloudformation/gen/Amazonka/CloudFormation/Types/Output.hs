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
-- Module      : Amazonka.CloudFormation.Types.Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Output data type.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | The value associated with the output.
    outputValue :: Prelude.Maybe Prelude.Text,
    -- | The key associated with the output.
    outputKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the export associated with the output.
    exportName :: Prelude.Maybe Prelude.Text,
    -- | User defined description associated with the output.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputValue', 'output_outputValue' - The value associated with the output.
--
-- 'outputKey', 'output_outputKey' - The key associated with the output.
--
-- 'exportName', 'output_exportName' - The name of the export associated with the output.
--
-- 'description', 'output_description' - User defined description associated with the output.
newOutput ::
  Output
newOutput =
  Output'
    { outputValue = Prelude.Nothing,
      outputKey = Prelude.Nothing,
      exportName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The value associated with the output.
output_outputValue :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_outputValue = Lens.lens (\Output' {outputValue} -> outputValue) (\s@Output' {} a -> s {outputValue = a} :: Output)

-- | The key associated with the output.
output_outputKey :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_outputKey = Lens.lens (\Output' {outputKey} -> outputKey) (\s@Output' {} a -> s {outputKey = a} :: Output)

-- | The name of the export associated with the output.
output_exportName :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_exportName = Lens.lens (\Output' {exportName} -> exportName) (\s@Output' {} a -> s {exportName = a} :: Output)

-- | User defined description associated with the output.
output_description :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_description = Lens.lens (\Output' {description} -> description) (\s@Output' {} a -> s {description = a} :: Output)

instance Core.FromXML Output where
  parseXML x =
    Output'
      Prelude.<$> (x Core..@? "OutputValue")
      Prelude.<*> (x Core..@? "OutputKey")
      Prelude.<*> (x Core..@? "ExportName")
      Prelude.<*> (x Core..@? "Description")

instance Prelude.Hashable Output where
  hashWithSalt salt' Output' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` exportName
      `Prelude.hashWithSalt` outputKey
      `Prelude.hashWithSalt` outputValue

instance Prelude.NFData Output where
  rnf Output' {..} =
    Prelude.rnf outputValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf exportName
      `Prelude.seq` Prelude.rnf outputKey
