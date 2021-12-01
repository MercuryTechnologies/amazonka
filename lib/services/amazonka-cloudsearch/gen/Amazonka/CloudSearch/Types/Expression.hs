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
-- Module      : Amazonka.CloudSearch.Types.Expression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.Expression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A named expression that can be evaluated at search time. Can be used to
-- sort the search results, define other expressions, or return computed
-- information in the search results.
--
-- /See:/ 'newExpression' smart constructor.
data Expression = Expression'
  { expressionName :: Prelude.Text,
    expressionValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Expression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expressionName', 'expression_expressionName' - Undocumented member.
--
-- 'expressionValue', 'expression_expressionValue' - Undocumented member.
newExpression ::
  -- | 'expressionName'
  Prelude.Text ->
  -- | 'expressionValue'
  Prelude.Text ->
  Expression
newExpression pExpressionName_ pExpressionValue_ =
  Expression'
    { expressionName = pExpressionName_,
      expressionValue = pExpressionValue_
    }

-- | Undocumented member.
expression_expressionName :: Lens.Lens' Expression Prelude.Text
expression_expressionName = Lens.lens (\Expression' {expressionName} -> expressionName) (\s@Expression' {} a -> s {expressionName = a} :: Expression)

-- | Undocumented member.
expression_expressionValue :: Lens.Lens' Expression Prelude.Text
expression_expressionValue = Lens.lens (\Expression' {expressionValue} -> expressionValue) (\s@Expression' {} a -> s {expressionValue = a} :: Expression)

instance Core.FromXML Expression where
  parseXML x =
    Expression'
      Prelude.<$> (x Core..@ "ExpressionName")
      Prelude.<*> (x Core..@ "ExpressionValue")

instance Prelude.Hashable Expression where
  hashWithSalt salt' Expression' {..} =
    salt' `Prelude.hashWithSalt` expressionValue
      `Prelude.hashWithSalt` expressionName

instance Prelude.NFData Expression where
  rnf Expression' {..} =
    Prelude.rnf expressionName
      `Prelude.seq` Prelude.rnf expressionValue

instance Core.ToQuery Expression where
  toQuery Expression' {..} =
    Prelude.mconcat
      [ "ExpressionName" Core.=: expressionName,
        "ExpressionValue" Core.=: expressionValue
      ]
